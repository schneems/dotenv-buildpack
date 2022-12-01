use libcnb::build::{BuildContext, BuildResult, BuildResultBuilder};
use libcnb::data::build_plan::BuildPlanBuilder;
use libcnb::data::layer_name;
use libcnb::detect::{DetectContext, DetectResult, DetectResultBuilder};
use libcnb::generic::{GenericError, GenericMetadata, GenericPlatform};
use libcnb::{buildpack_main, Buildpack};

use crate::layers::env_override_layer::EnvOverrideLayer;

mod layers;
mod parser;

pub(crate) struct DotenvBuildpack;

impl Buildpack for DotenvBuildpack {
    // The CNB platform this buildpack targets, usually `GenericPlatform`. See the CNB spec for
    // more information about platforms:
    // https://github.com/buildpacks/spec/blob/main/buildpack.md
    type Platform = GenericPlatform;

    // The type for the metadata of the buildpack itself. This is the data found in the
    // `[metadata]` section of your buildpack's `buildpack.toml`. The framework will automatically
    // try to parse it into the specified type. This example buildpack uses GenericMetadata which
    // provides low-level access to the TOML table.
    type Metadata = GenericMetadata;

    // The error type for this buildpack. Buildpack authors usually implement an enum with
    // specific errors that can happen during buildpack execution. This error type should
    // only contain error specific to this buildpack, such as `CouldNotExecuteMaven` or
    // `InvalidGemfileLock`. This example buildpack uses `GenericError` which means this buildpack
    // does not specify any errors.
    //
    // Common errors that happen during buildpack execution such as I/O errors while
    // writing CNB TOML files are handled by libcnb.rs itself.
    type Error = GenericError;

    // This method will be called when the CNB lifecycle executes the detect phase (`bin/detect`).
    // Use the `DetectContext` to access CNB data such as the stack this buildpack is currently
    // executed on, the app directory and similar things. When using libcnb.rs, you never have
    // to read environment variables or read/write files to disk to interact with the CNB lifecycle.
    //
    // One example of this is the return type of this method. `DetectResult` encapsulates the
    // required exit code as well as the data written to the build plan. libcnb.rs will,
    // according to the returned value, handle both writing the build plan and exiting with
    // the correct status code for you.
    fn detect(&self, context: DetectContext<Self>) -> libcnb::Result<DetectResult, Self::Error> {
        let mut plan_builder = BuildPlanBuilder::new().provides("dotenv");

        if context.app_dir.join(".env").exists() {
            plan_builder = plan_builder.requires("dotenv");
        }

        DetectResultBuilder::pass()
            .build_plan(plan_builder.build())
            .build()
    }

    // Similar to detect, this method will be called when the CNB lifecycle executes the
    // build phase (`bin/build`).
    fn build(&self, context: BuildContext<Self>) -> libcnb::Result<BuildResult, Self::Error> {
        println!("---> DotEnv Buildpack");

        let contents =
            fs_err::read_to_string(context.app_dir.join(".env")).expect("Error reading .env file");

        let env_vector = match parser::get_env(&contents) {
            Ok((_, env_vector)) => env_vector,
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                println!(
                    "Error parsing `.env` file:\n\n{}",
                    nom::error::convert_error(contents.as_str(), e)
                );
                std::process::exit(1);
            }
            Err(nom::Err::Incomplete(_)) => {
                unreachable!("Internal error: We are using nom-complete everywhere and not streaming, this should never happen")
            }
        };

        for (key, value) in env_vector.iter() {
            println!(
                "  export {}=<{} character(s)>",
                key.to_string_lossy(),
                value.to_string_lossy().chars().count()
            );
        }

        context.handle_layer(layer_name!("dotenv"), EnvOverrideLayer::new(env_vector))?;

        BuildResultBuilder::new().build()
    }
}

// Implements the main function and wires up the framework for the given buildpack.
buildpack_main!(DotenvBuildpack);
