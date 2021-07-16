# ink! CI for Linux Distributions

This Docker image is based on: https://github.com/paritytech/scripts/blob/master/dockerfiles/ink-ci-linux/Dockerfile

It is modified in a way such that it provides a prebuild ink! Smart Contract in order to minimize build time for standard ink contracts without extra dependencies.

## Usage

While being within the `Dockerfile` directory, build it with:

`sudo docker build -t ink-ci-linux .`

Then, to compile a ink! Smart Contract file `lib.rs`, enter:

`docker run --workdir /builds/contract --volume $local_path_to_source$/lib.rs:/builds/contract/lib.rs --volume $local_path_to_result$:/builds/contract/target/ink ink-backend cargo contract build`

where you should replace `$local_path_to_source$` to the path on your local machine pointing to `lib.rs` and `$local_path_to_result$` by your desired output path for the resulting `.contract`, `.wasm` and `.json` files.
