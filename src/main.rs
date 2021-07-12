#![deny(rust_2018_idioms)]

use iron::{
    headers::ContentType,
    modifiers::Header,
    prelude::*,
    status,
};
use mount::Mount;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use snafu::{ResultExt, Snafu};
use std::{
    any::Any,
    convert::{TryFrom, TryInto},
    env,
};

use crate::sandbox::Sandbox;

const DEFAULT_ADDRESS: &str = "127.0.0.1";
const DEFAULT_PORT: u16 = 5000;

mod asm_cleanup;
mod sandbox;

fn main() {
    let address = env::var("PLAYGROUND_UI_ADDRESS").unwrap_or_else(|_| DEFAULT_ADDRESS.to_string());
    let port = env::var("PLAYGROUND_UI_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(DEFAULT_PORT);

    let mut mount = Mount::new();
    mount.mount("/compile", compile);
    let chain = Chain::new(mount);

    log::info!("Starting the server on http://{}:{}", address, port);
    Iron::new(chain)
        .http((&*address, port))
        .expect("Unable to start server");
}

fn compile(req: &mut Request<'_, '_>) -> IronResult<Response> {
    with_sandbox(req, |sandbox, req: CompileRequest| {
        let req = req.try_into()?;
        let body = |req| sandbox.compile(&req);
        let response = body(req);
        response.map(CompileResponse::from).context(Compilation)
    })
}

fn with_sandbox<Req, Resp, F>(req: &mut Request<'_, '_>, f: F) -> IronResult<Response>
where
    F: FnOnce(Sandbox, Req) -> Result<Resp>,
    Req: DeserializeOwned + Clone + Any + 'static,
    Resp: serde::Serialize,
{
    serialize_to_response(run_handler(req, f))
}

fn run_handler<Req, Resp, F>(req: &mut Request<'_, '_>, f: F) -> Result<Resp>
where
    F: FnOnce(Sandbox, Req) -> Result<Resp>,
    Req: DeserializeOwned + Clone + Any + 'static,
{
    deserialize_from_request(req, |req| {
        let sandbox = Sandbox::new().context(SandboxCreation)?;
        f(sandbox, req)
    })
}

fn deserialize_from_request<Req, Resp, F>(req: &mut Request<'_, '_>, f: F) -> Result<Resp>
where
    F: FnOnce(Req) -> Result<Resp>,
    Req: DeserializeOwned + Clone + Any + 'static,
{
    let body = req
        .get::<bodyparser::Struct<Req>>()
        .context(Deserialization)?;

    let req = body.ok_or(Error::RequestMissing)?;

    let resp = f(req)?;

    Ok(resp)
}

fn serialize_to_response<Resp>(response: Result<Resp>) -> IronResult<Response>
where
    Resp: serde::Serialize,
{
    let response = response.and_then(|resp| {
        let resp = serde_json::ser::to_string(&resp).context(Serialization)?;
        Ok(resp)
    });

    match response {
        Ok(body) => Ok(Response::with((
            status::Ok,
            Header(ContentType::json()),
            body,
        ))),
        Err(err) => {
            let err = ErrorJson {
                error: err.to_string(),
            };
            match serde_json::ser::to_string(&err) {
                Ok(error_str) => Ok(Response::with((
                    status::InternalServerError,
                    Header(ContentType::json()),
                    error_str,
                ))),
                Err(_) => Ok(Response::with((
                    status::InternalServerError,
                    Header(ContentType::json()),
                    FATAL_ERROR_JSON,
                ))),
            }
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Sandbox creation failed: {}", source))]
    SandboxCreation { source: sandbox::Error },
    #[snafu(display("Compilation operation failed: {}", source))]
    Compilation { source: sandbox::Error },
    #[snafu(display("Execution operation failed: {}", source))]
    Execution { source: sandbox::Error },
    #[snafu(display("Evaluation operation failed: {}", source))]
    Evaluation { source: sandbox::Error },
    #[snafu(display("Linting operation failed: {}", source))]
    Linting { source: sandbox::Error },
    #[snafu(display("Expansion operation failed: {}", source))]
    Expansion { source: sandbox::Error },
    #[snafu(display("Formatting operation failed: {}", source))]
    Formatting { source: sandbox::Error },
    #[snafu(display("Interpreting operation failed: {}", source))]
    Interpreting { source: sandbox::Error },
    #[snafu(display("Caching operation failed: {}", source))]
    Caching { source: sandbox::Error },
    #[snafu(display("Unable to serialize response: {}", source))]
    Serialization { source: serde_json::Error },
    #[snafu(display("Unable to deserialize request: {}", source))]
    Deserialization { source: bodyparser::BodyError },
    #[snafu(display("The value {:?} is not a valid target", value))]
    InvalidTarget { value: String },
    #[snafu(display("The value {:?} is not a valid assembly flavor", value))]
    InvalidAssemblyFlavor { value: String },
    #[snafu(display("The value {:?} is not a valid demangle option", value))]
    InvalidDemangleAssembly { value: String },
    #[snafu(display("The value {:?} is not a valid assembly processing option", value))]
    InvalidProcessAssembly { value: String },
    #[snafu(display("The value {:?} is not a valid channel", value,))]
    InvalidChannel { value: String },
    #[snafu(display("The value {:?} is not a valid mode", value))]
    InvalidMode { value: String },
    #[snafu(display("The value {:?} is not a valid edition", value))]
    InvalidEdition { value: String },
    #[snafu(display("The value {:?} is not a valid crate type", value))]
    InvalidCrateType { value: String },
    #[snafu(display("No request was provided"))]
    RequestMissing,
    #[snafu(display("The cache has been poisoned"))]
    CachePoisoned,
}

type Result<T, E = Error> = ::std::result::Result<T, E>;

const FATAL_ERROR_JSON: &str =
    r#"{"error": "Multiple cascading errors occurred, abandon all hope"}"#;

#[derive(Debug, Clone, Serialize)]
struct ErrorJson {
    error: String,
}

#[derive(Debug, Clone, Deserialize)]
struct CompileRequest {
    target: String,
    #[serde(rename = "assemblyFlavor")]
    assembly_flavor: Option<String>,
    #[serde(rename = "demangleAssembly")]
    demangle_assembly: Option<String>,
    #[serde(rename = "processAssembly")]
    process_assembly: Option<String>,
    channel: String,
    mode: String,
    #[serde(default)]
    edition: String,
    #[serde(rename = "crateType")]
    crate_type: String,
    tests: bool,
    #[serde(default)]
    backtrace: bool,
    code: String,
}

#[derive(Debug, Clone, Serialize)]
struct CompileResponse {
    success: bool,
    code: String,
    stdout: String,
    stderr: String,
}

impl From<sandbox::CompileResponse> for CompileResponse {
    fn from(me: sandbox::CompileResponse) -> Self {
        CompileResponse {
            success: me.success,
            code: me.code,
            stdout: me.stdout,
            stderr: me.stderr,
        }
    }
}

impl TryFrom<CompileRequest> for sandbox::CompileRequest {
    type Error = Error;

    fn try_from(me: CompileRequest) -> Result<Self> {
        let target = parse_target(&me.target)?;
        let assembly_flavor = match me.assembly_flavor {
            Some(f) => Some(parse_assembly_flavor(&f)?),
            None => None,
        };

        let demangle = match me.demangle_assembly {
            Some(f) => Some(parse_demangle_assembly(&f)?),
            None => None,
        };

        let process_assembly = match me.process_assembly {
            Some(f) => Some(parse_process_assembly(&f)?),
            None => None,
        };

        let target = match (target, assembly_flavor, demangle, process_assembly) {
            (
                sandbox::CompileTarget::Assembly(_, _, _),
                Some(flavor),
                Some(demangle),
                Some(process),
            ) => sandbox::CompileTarget::Assembly(flavor, demangle, process),
            _ => target,
        };

        Ok(sandbox::CompileRequest {
            target,
            channel: parse_channel(&me.channel)?,
            mode: parse_mode(&me.mode)?,
            edition: parse_edition(&me.edition)?,
            crate_type: parse_crate_type(&me.crate_type)?,
            tests: me.tests,
            backtrace: me.backtrace,
            code: me.code,
        })
    }
}

fn parse_target(s: &str) -> Result<sandbox::CompileTarget> {
    Ok(match s {
        "asm" => sandbox::CompileTarget::Assembly(
            sandbox::AssemblyFlavor::Att,
            sandbox::DemangleAssembly::Demangle,
            sandbox::ProcessAssembly::Filter,
        ),
        "llvm-ir" => sandbox::CompileTarget::LlvmIr,
        "mir" => sandbox::CompileTarget::Mir,
        "hir" => sandbox::CompileTarget::Hir,
        "wasm" => sandbox::CompileTarget::Wasm,
        value => InvalidTarget { value }.fail()?,
    })
}

fn parse_assembly_flavor(s: &str) -> Result<sandbox::AssemblyFlavor> {
    Ok(match s {
        "att" => sandbox::AssemblyFlavor::Att,
        "intel" => sandbox::AssemblyFlavor::Intel,
        value => InvalidAssemblyFlavor { value }.fail()?,
    })
}

fn parse_demangle_assembly(s: &str) -> Result<sandbox::DemangleAssembly> {
    Ok(match s {
        "demangle" => sandbox::DemangleAssembly::Demangle,
        "mangle" => sandbox::DemangleAssembly::Mangle,
        value => InvalidDemangleAssembly { value }.fail()?,
    })
}

fn parse_process_assembly(s: &str) -> Result<sandbox::ProcessAssembly> {
    Ok(match s {
        "filter" => sandbox::ProcessAssembly::Filter,
        "raw" => sandbox::ProcessAssembly::Raw,
        value => InvalidProcessAssembly { value }.fail()?,
    })
}

fn parse_channel(s: &str) -> Result<sandbox::Channel> {
    Ok(match s {
        "stable" => sandbox::Channel::Stable,
        "beta" => sandbox::Channel::Beta,
        "nightly" => sandbox::Channel::Nightly,
        value => InvalidChannel { value }.fail()?,
    })
}

fn parse_mode(s: &str) -> Result<sandbox::Mode> {
    Ok(match s {
        "debug" => sandbox::Mode::Debug,
        "release" => sandbox::Mode::Release,
        value => InvalidMode { value }.fail()?,
    })
}

fn parse_edition(s: &str) -> Result<Option<sandbox::Edition>> {
    Ok(match s {
        "" => None,
        "2015" => Some(sandbox::Edition::Rust2015),
        "2018" => Some(sandbox::Edition::Rust2018),
        "2021" => Some(sandbox::Edition::Rust2021),
        value => InvalidEdition { value }.fail()?,
    })
}

fn parse_crate_type(s: &str) -> Result<sandbox::CrateType> {
    use crate::sandbox::{CrateType::*, LibraryType::*};
    Ok(match s {
        "bin" => Binary,
        "lib" => Library(Lib),
        "dylib" => Library(Dylib),
        "rlib" => Library(Rlib),
        "staticlib" => Library(Staticlib),
        "cdylib" => Library(Cdylib),
        "proc-macro" => Library(ProcMacro),
        value => InvalidCrateType { value }.fail()?,
    })
}
