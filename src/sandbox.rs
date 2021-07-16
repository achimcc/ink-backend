use serde_derive::Deserialize;
use snafu::{OptionExt, ResultExt, Snafu};
use std::{
    ffi::OsStr,
    fmt,
    fs::{self, File},
    io::{self, prelude::*, BufReader, ErrorKind},
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
    string,
    time::Duration,
};
use tempdir::TempDir;
use tokio::process::Command;

const DOCKER_PROCESS_TIMEOUT_HARD: Duration = Duration::from_secs(12);

#[derive(Debug, Deserialize)]
struct CrateInformationInner {
    name: String,
    version: String,
    id: String,
}

#[derive(Debug, Clone)]
pub struct CrateInformation {
    pub name: String,
    pub version: String,
    pub id: String,
}

impl From<CrateInformationInner> for CrateInformation {
    fn from(me: CrateInformationInner) -> Self {
        let CrateInformationInner { name, version, id } = me;
        Self { name, version, id }
    }
}

#[derive(Debug, Clone)]
pub struct Version {
    pub release: String,
    pub commit_hash: String,
    pub commit_date: String,
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Unable to create temporary directory: {}", source))]
    UnableToCreateTempDir { source: io::Error },
    #[snafu(display("Unable to create output directory: {}", source))]
    UnableToCreateOutputDir { source: io::Error },
    #[snafu(display("Unable to set permissions for output directory: {}", source))]
    UnableToSetOutputPermissions { source: io::Error },
    #[snafu(display("Unable to create source file: {}", source))]
    UnableToCreateSourceFile { source: io::Error },
    #[snafu(display("Unable to set permissions for source file: {}", source))]
    UnableToSetSourcePermissions { source: io::Error },

    #[snafu(display("Unable to start the compiler: {}", source))]
    UnableToStartCompiler { source: io::Error },
    #[snafu(display("Unable to find the compiler ID"))]
    MissingCompilerId,
    #[snafu(display("Unable to wait for the compiler: {}", source))]
    UnableToWaitForCompiler { source: io::Error },
    #[snafu(display("Unable to get output from the compiler: {}", source))]
    UnableToGetOutputFromCompiler { source: io::Error },
    #[snafu(display("Unable to remove the compiler: {}", source))]
    UnableToRemoveCompiler { source: io::Error },
    #[snafu(display("Compiler execution took longer than {} ms", timeout.as_millis()))]
    CompilerExecutionTimedOut {
        source: tokio::time::Elapsed,
        timeout: Duration,
    },

    #[snafu(display("Unable to read output file: {}", source))]
    UnableToReadOutput { source: io::Error },
    #[snafu(display("Unable to read crate information: {}", source))]
    UnableToParseCrateInformation { source: ::serde_json::Error },
    #[snafu(display("Output was not valid UTF-8: {}", source))]
    OutputNotUtf8 { source: string::FromUtf8Error },
    #[snafu(display("Output was missing"))]
    OutputMissing,
    #[snafu(display("Release was missing from the version output"))]
    VersionReleaseMissing,
    #[snafu(display("Commit hash was missing from the version output"))]
    VersionHashMissing,
    #[snafu(display("Commit date was missing from the version output"))]
    VersionDateMissing,
}

pub type Result<T, E = Error> = ::std::result::Result<T, E>;

pub struct Sandbox {
    #[allow(dead_code)]
    scratch: TempDir,
    input_file: PathBuf,
    output_dir: PathBuf,
}

fn vec_to_str(v: Vec<u8>) -> Result<String> {
    String::from_utf8(v).context(OutputNotUtf8)
}

// We must create a world-writable files (rustfmt) and directories
// (LLVM IR) so that the process inside the Docker container can write
// into it.
//
// This problem does *not* occur when using the indirection of
// docker-machine.
fn wide_open_permissions() -> std::fs::Permissions {
    PermissionsExt::from_mode(0o777)
}

impl Sandbox {
    pub fn new() -> Result<Self> {
        let scratch = TempDir::new("playground").context(UnableToCreateTempDir)?;
        let input_file = scratch.path().join("input.rs");
        let output_dir = scratch.path().join("output");

        fs::create_dir(&output_dir).context(UnableToCreateOutputDir)?;
        fs::set_permissions(&output_dir, wide_open_permissions())
            .context(UnableToSetOutputPermissions)?;

        Ok(Sandbox {
            scratch,
            input_file,
            output_dir,
        })
    }

    pub fn compile(&self, req: &CompileRequest) -> Result<CompileResponse> {
        self.write_source_code(&req.code)?;

        let command = self.compile_command(req.channel);

        let output = run_command_with_timeout(command)?;

        // The compiler writes the file to a name like
        // `compilation-3b75174cac3d47fb.ll`, so we just find the
        // first with the right extension.
        let file = fs::read_dir(&self.output_dir)
            .context(UnableToReadOutput)?
            .flat_map(|entry| entry)
            .map(|entry| entry.path())
            .find(|path| path.extension() == Some(req.target.extension()));

        let stdout = vec_to_str(output.stdout)?;
        let mut stderr = vec_to_str(output.stderr)?;

        let code = match file {
            Some(file) => read(&file)?,
            None => {
                // If we didn't find the file, it's *most* likely that
                // the user's code was invalid. Tack on our own error
                // to the compiler's error instead of failing the
                // request.
                use self::fmt::Write;
                write!(
                    &mut stderr,
                    "\nUnable to locate file for {} output",
                    req.target
                )
                .expect("Unable to write to a string");
                None
            }
        };

        let code = code.unwrap();

        Ok(CompileResponse {
            success: output.status.success(),
            code,
            stdout,
            stderr,
        })
    }

    fn write_source_code(&self, code: &str) -> Result<()> {
        fs::write(&self.input_file, code).context(UnableToCreateSourceFile)?;
        fs::set_permissions(&self.input_file, wide_open_permissions())
            .context(UnableToSetSourcePermissions)?;

        log::debug!(
            "Wrote {} bytes of source to {}",
            code.len(),
            self.input_file.display()
        );
        Ok(())
    }

    fn compile_command(
        &self,
        channel: Channel,
    ) -> Command {
        let mut cmd = self.docker_command();

        let execution_cmd = build_execution_command();

        cmd.arg(&channel.container_name()).args(&execution_cmd);

        log::debug!("Compilation command is {:?}", cmd);

        cmd
    }

    fn docker_command(&self) -> Command {
        let crate_type = CrateType::Library(LibraryType::Lib);

        let mut mount_input_file = self.input_file.as_os_str().to_os_string();
        mount_input_file.push(":");
        mount_input_file.push("/builds/contract/");
        mount_input_file.push(crate_type.file_name());

        let mut mount_output_dir = self.output_dir.as_os_str().to_os_string();
        mount_output_dir.push(":");
        mount_output_dir.push("/builds/contract/target/ink");

        let mut cmd = basic_secure_docker_command();

        cmd.arg("--volume")
            .arg(&mount_input_file)
            .arg("--volume")
            .arg(&mount_output_dir);

        cmd
    }
}

macro_rules! docker_command {
    ($($arg:expr),* $(,)?) => ({
        let mut cmd = Command::new("docker");
        $( cmd.arg($arg); )*
        cmd
    });
}

fn basic_secure_docker_command() -> Command {
    let mut cmd = docker_command!(
        "run",
        "--workdir",
        "/builds/contract",
    );

    cmd.kill_on_drop(true);

    cmd
}

fn build_execution_command() -> Vec<&'static str> {

    let cmd = vec!["cargo", "contract", "build"];

    cmd
}

fn read(path: &Path) -> Result<Option<Vec<u8>>> {
    let f = match File::open(path) {
        Ok(f) => f,
        Err(ref e) if e.kind() == ErrorKind::NotFound => return Ok(None),
        e => e.context(UnableToReadOutput)?,
    };
    let mut f = BufReader::new(f);
    let metadata = fs::metadata(path).expect("unable to read metadata");
    // f.read_to_string(&mut s).context(UnableToReadOutput)?;
    let mut buffer = vec![0; metadata.len() as usize];
    f.read(&mut buffer).expect("buffer overflow");
    Ok(Some(buffer))
}

#[tokio::main]
async fn run_command_with_timeout(mut command: Command) -> Result<std::process::Output> {
    use std::os::unix::process::ExitStatusExt;

    let timeout = DOCKER_PROCESS_TIMEOUT_HARD;

    let output = command.output().await.context(UnableToStartCompiler)?;

    // Exit early, in case we don't have the container
    /*
    if !output.status.success() {
        return Ok(output);
    }
    */ 
    let output = String::from_utf8_lossy(&output.stdout);
    let id = output.lines().next().context(MissingCompilerId)?.trim();

    // ----------

    let mut command = docker_command!("wait", id);

    let timed_out = match tokio::time::timeout(timeout, command.output()).await {
        Ok(Ok(o)) => {
            // Didn't time out, didn't fail to run
            let o = String::from_utf8_lossy(&o.stdout);
            let code = o
                .lines()
                .next()
                .unwrap_or("")
                .trim()
                .parse()
                .unwrap_or(i32::MAX);
            Ok(ExitStatusExt::from_raw(code))
        }
        Ok(e) => return e.context(UnableToWaitForCompiler), // Failed to run
        Err(e) => Err(e),                                   // Timed out
    };

    // ----------

    let mut command = docker_command!("logs", id);
    let mut output = command
        .output()
        .await
        .context(UnableToGetOutputFromCompiler)?;

    // ----------

    let mut command = docker_command!(
        "rm", // Kills container if still running
        "--force", id
    );
    command.stdout(std::process::Stdio::null());
    command.status().await.context(UnableToRemoveCompiler)?;

    let code = timed_out.context(CompilerExecutionTimedOut { timeout })?;

    output.status = code;

    Ok(output)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssemblyFlavor {
    Att,
    Intel,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DemangleAssembly {
    Demangle,
    Mangle,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ProcessAssembly {
    Filter,
    Raw,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::IntoStaticStr)]
pub enum CompileTarget {
    Assembly(AssemblyFlavor, DemangleAssembly, ProcessAssembly),
    LlvmIr,
    Mir,
    Hir,
    Wasm,
    Contract,
}

impl CompileTarget {
    fn extension(&self) -> &'static OsStr {
        let ext = match *self {
            CompileTarget::Assembly(_, _, _) => "s",
            CompileTarget::LlvmIr => "ll",
            CompileTarget::Mir => "mir",
            CompileTarget::Hir => "hir",
            CompileTarget::Wasm => "wat",
            CompileTarget::Contract => "contract",
        };
        OsStr::new(ext)
    }
}

impl fmt::Display for CompileTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::CompileTarget::*;

        match *self {
            Assembly(_, _, _) => "assembly".fmt(f),
            LlvmIr => "LLVM IR".fmt(f),
            Mir => "Rust MIR".fmt(f),
            Hir => "Rust HIR".fmt(f),
            Wasm => "WebAssembly".fmt(f),
            Contract => "Contract".fmt(f),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::IntoStaticStr)]
pub enum Channel {
    Stable,
    Beta,
    Nightly,
}

impl Channel {
    fn container_name(&self) -> &'static str {
    /*    
        use self::Channel::*;
        
        match *self {
            Stable => "rust-stable",
            Beta => "rust-beta",
            Nightly => "rust-nightly",
        }
    */
    "ink-backend"
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::IntoStaticStr)]
pub enum Mode {
    Debug,
    Release,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::IntoStaticStr)]
pub enum Edition {
    Rust2015,
    Rust2018,
    Rust2021, // TODO - add parallel tests for 2021
}

impl Edition {
    fn cargo_ident(&self) -> &'static str {
        use self::Edition::*;

        match *self {
            Rust2015 => "2015",
            Rust2018 => "2018",
            Rust2021 => "2021",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::IntoStaticStr)]
pub enum CrateType {
    Binary,
    Library(LibraryType),
}

impl CrateType {
    fn file_name(&self) -> &'static str {
        use self::CrateType::*;

        match *self {
            Binary => "main.rs",
            Library(_) => "lib.rs",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, strum::IntoStaticStr)]
pub enum LibraryType {
    Lib,
    Dylib,
    Rlib,
    Staticlib,
    Cdylib,
    ProcMacro,
}

impl LibraryType {
    fn cargo_ident(&self) -> &'static str {
        use self::LibraryType::*;

        match *self {
            Lib => "lib",
            Dylib => "dylib",
            Rlib => "rlib",
            Staticlib => "staticlib",
            Cdylib => "cdylib",
            ProcMacro => "proc-macro",
        }
    }
}

trait DockerCommandExt {
    fn apply_crate_type(&mut self, req: impl CrateTypeRequest);
    fn apply_edition(&mut self, req: impl EditionRequest);
    fn apply_backtrace(&mut self, req: impl BacktraceRequest);
}

impl DockerCommandExt for Command {
    fn apply_crate_type(&mut self, req: impl CrateTypeRequest) {
        if let CrateType::Library(lib) = req.crate_type() {
            self.args(&[
                "--env",
                &format!("PLAYGROUND_CRATE_TYPE={}", lib.cargo_ident()),
            ]);
        }
    }

    fn apply_edition(&mut self, req: impl EditionRequest) {
        if let Some(edition) = req.edition() {
            if edition == Edition::Rust2021 {
                self.args(&["--env", &format!("PLAYGROUND_FEATURE_EDITION2021=true")]);
            }

            self.args(&[
                "--env",
                &format!("PLAYGROUND_EDITION={}", edition.cargo_ident()),
            ]);
        }
    }

    fn apply_backtrace(&mut self, req: impl BacktraceRequest) {
        if req.backtrace() {
            self.args(&["--env", "RUST_BACKTRACE=1"]);
        }
    }
}

trait CrateTypeRequest {
    fn crate_type(&self) -> CrateType;
}

impl<R: CrateTypeRequest> CrateTypeRequest for &'_ R {
    fn crate_type(&self) -> CrateType {
        (*self).crate_type()
    }
}

trait EditionRequest {
    fn edition(&self) -> Option<Edition>;
}

impl<R: EditionRequest> EditionRequest for &'_ R {
    fn edition(&self) -> Option<Edition> {
        (*self).edition()
    }
}

trait BacktraceRequest {
    fn backtrace(&self) -> bool;
}

impl<R: BacktraceRequest> BacktraceRequest for &'_ R {
    fn backtrace(&self) -> bool {
        (*self).backtrace()
    }
}

#[derive(Debug, Clone)]
pub struct CompileRequest {
    pub target: CompileTarget,
    pub channel: Channel,
    pub crate_type: CrateType,
    pub mode: Mode,
    pub edition: Option<Edition>,
    pub tests: bool,
    pub backtrace: bool,
    pub code: String,
}

impl CrateTypeRequest for CompileRequest {
    fn crate_type(&self) -> CrateType {
        self.crate_type
    }
}

impl EditionRequest for CompileRequest {
    fn edition(&self) -> Option<Edition> {
        self.edition
    }
}

impl BacktraceRequest for CompileRequest {
    fn backtrace(&self) -> bool {
        self.backtrace
    }
}

#[derive(Debug, Clone)]
pub struct CompileResponse {
    pub success: bool,
    pub code: Vec<u8>,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, Clone)]
pub struct ExecuteRequest {
    pub channel: Channel,
    pub mode: Mode,
    pub edition: Option<Edition>,
    pub crate_type: CrateType,
    pub tests: bool,
    pub backtrace: bool,
    pub code: String,
}

impl CrateTypeRequest for ExecuteRequest {
    fn crate_type(&self) -> CrateType {
        self.crate_type
    }
}

impl EditionRequest for ExecuteRequest {
    fn edition(&self) -> Option<Edition> {
        self.edition
    }
}

impl BacktraceRequest for ExecuteRequest {
    fn backtrace(&self) -> bool {
        self.backtrace
    }
}

#[derive(Debug, Clone)]
pub struct ExecuteResponse {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, Clone)]
pub struct FormatRequest {
    pub code: String,
    pub edition: Option<Edition>,
}

impl EditionRequest for FormatRequest {
    fn edition(&self) -> Option<Edition> {
        self.edition
    }
}

#[derive(Debug, Clone)]
pub struct FormatResponse {
    pub success: bool,
    pub code: String,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, Clone)]
pub struct ClippyRequest {
    pub code: String,
    pub edition: Option<Edition>,
    pub crate_type: CrateType,
}

impl CrateTypeRequest for ClippyRequest {
    fn crate_type(&self) -> CrateType {
        self.crate_type
    }
}

impl EditionRequest for ClippyRequest {
    fn edition(&self) -> Option<Edition> {
        self.edition
    }
}

#[derive(Debug, Clone)]
pub struct ClippyResponse {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, Clone)]
pub struct MiriRequest {
    pub code: String,
    pub edition: Option<Edition>,
}

impl EditionRequest for MiriRequest {
    fn edition(&self) -> Option<Edition> {
        self.edition
    }
}

#[derive(Debug, Clone)]
pub struct MiriResponse {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
}

#[derive(Debug, Clone)]
pub struct MacroExpansionRequest {
    pub code: String,
    pub edition: Option<Edition>,
}

impl EditionRequest for MacroExpansionRequest {
    fn edition(&self) -> Option<Edition> {
        self.edition
    }
}

#[derive(Debug, Clone)]
pub struct MacroExpansionResponse {
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
}

#[cfg(test)]
mod test {
    use super::*;

    // Running the tests completely in parallel causes spurious
    // failures due to my resource-limited Docker
    // environment. Additionally, we have some tests that *require*
    // that no other Docker processes are running.
    fn one_test_at_a_time() -> impl Drop {
        use lazy_static::lazy_static;
        use std::sync::Mutex;

        lazy_static! {
            static ref DOCKER_SINGLETON: Mutex<()> = Default::default();
        }

        // We can't poison the empty tuple
        DOCKER_SINGLETON.lock().unwrap_or_else(|e| e.into_inner())
    }

    const HELLO_WORLD_CODE: &'static str = r#"
    fn main() {
        println!("Hello, world!");
    }
    "#;

    impl Default for ExecuteRequest {
        fn default() -> Self {
            ExecuteRequest {
                channel: Channel::Stable,
                crate_type: CrateType::Binary,
                mode: Mode::Debug,
                tests: false,
                code: HELLO_WORLD_CODE.to_string(),
                edition: None,
                backtrace: false,
            }
        }
    }

    impl Default for CompileRequest {
        fn default() -> Self {
            CompileRequest {
                target: CompileTarget::LlvmIr,
                channel: Channel::Stable,
                crate_type: CrateType::Binary,
                mode: Mode::Debug,
                tests: false,
                code: HELLO_WORLD_CODE.to_string(),
                edition: None,
                backtrace: false,
            }
        }
    }

    impl Default for ClippyRequest {
        fn default() -> Self {
            ClippyRequest {
                code: HELLO_WORLD_CODE.to_string(),
                crate_type: CrateType::Binary,
                edition: None,
            }
        }
    }

}
    /*

    #[test]
    fn output_llvm_ir() {
        let _singleton = one_test_at_a_time();
        let req = CompileRequest {
            target: CompileTarget::LlvmIr,
            ..CompileRequest::default()
        };

        let sb = Sandbox::new().expect("Unable to create sandbox");
        let resp = sb.compile(&req).expect("Unable to compile code");

        assert!(resp.code.contains("ModuleID"));
        assert!(resp.code.contains("target datalayout"));
        assert!(resp.code.contains("target triple"));
    }

    #[test]
    fn output_assembly() {
        let _singleton = one_test_at_a_time();
        let req = CompileRequest {
            target: CompileTarget::Assembly(
                AssemblyFlavor::Att,
                DemangleAssembly::Mangle,
                ProcessAssembly::Raw,
            ),
            ..CompileRequest::default()
        };

        let sb = Sandbox::new().expect("Unable to create sandbox");
        let resp = sb.compile(&req).expect("Unable to compile code");

        assert!(resp.code.contains(".text"));
        assert!(resp.code.contains(".file"));
        assert!(resp.code.contains(".section"));
        assert!(resp.code.contains(".p2align"));
    }

    #[test]
    fn output_demangled_assembly() {
        let _singleton = one_test_at_a_time();
        let req = CompileRequest {
            target: CompileTarget::Assembly(
                AssemblyFlavor::Att,
                DemangleAssembly::Demangle,
                ProcessAssembly::Raw,
            ),
            ..CompileRequest::default()
        };

        let sb = Sandbox::new().expect("Unable to create sandbox");
        let resp = sb.compile(&req).expect("Unable to compile code");

        assert!(resp.code.contains("core::fmt::Arguments::new_v1"));
        assert!(resp.code.contains("std::io::stdio::_print@GOTPCREL"));
    }

    #[test]
    #[should_panic]
    fn output_filtered_assembly() {
        let _singleton = one_test_at_a_time();
        let req = CompileRequest {
            target: CompileTarget::Assembly(
                AssemblyFlavor::Att,
                DemangleAssembly::Mangle,
                ProcessAssembly::Filter,
            ),
            ..CompileRequest::default()
        };

        let sb = Sandbox::new().expect("Unable to create sandbox");
        let resp = sb.compile(&req).expect("Unable to compile code");

        assert!(resp.code.contains(".text"));
        assert!(resp.code.contains(".file"));
    }
}
*/