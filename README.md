## Dotenv Buildpack

Use locally to inject your environment variable stored locally in a `.env` file into values for use in your buildpack.

## What

The [12factorapp](https://12factor.net/) advocates for using environment variables to [configure](https://12factor.net/config) your application. In production this is commonly done with a secure service (such as Heroku's [config vars](https://devcenter.heroku.com/articles/config-vars)). Locally it's a little harder as environment variables will affect all processes, not just a single application.

To help solve this problem the concept of storing environment variables locally in a `.env` file was popularized.

- Ruby: https://github.com/bkeepers/dotenv
- Node: https://www.npmjs.com/package/dotenv

## Use

Put a `.env` in the root of your project and add it to your `.gitignore`, then put whatever environment variables you want read in when you `pack build` your project in there.

```
WEB_CONCURRENCY=2
```

Build your project with your usual buildpacks, but put dotenv first:

```
pack build <my-image-name> --buildpack heroku/dotenv --buildpack <your main buildpack here> /<path>/<to>/<app>
```

If it works you'll see some logging in your build output:

```
---> DotEnv Buildpack
  export WEB_CONCURRENCY=<1 character(s)>
```

Note: The values are hidden by default. To see the full value exported use `docker run -it --rm --entrypoint='/cnb/lifecycle/launcher' <my-image-name> env` to see all parsed values.

## Format

See the following examples to understand supported `.env` syntax:

### plain environment variable support:

A value followed by equal and a key. Each line is a new environment variable:

```
WEB_CONCURRENCY=6
SECRET_TOKEN=abcd-goldfish
```

You can also escape characters (such as space) with a slash:

```
SECRET_TOKEN=a\ bc
```

### environment variables starting with export:

```
export WEB_CONCURRENCY=6
export SECRET_TOKEN=abcd-goldfish
```

### Quoted variables

Supports values with multiple newlines:

```
MY_PRIVATE_KEY="----BEGIN PRIVATE KEY----
B60qrlm
q5a1bay
----END PRIVATE KEY----"
```

You can also use a newline in a quote:

```
MY_PRIVATE_KEY="----BEGIN PRIVATE KEY----\nB60qrlm\nq5a1bay\n----END PRIVATE KEY----"
```

## Detect

If `.env` is present it will also require `dotenv` to run itself (see build below). This buildpack alwasys provides `dotenv`.

## Build

Reads in the contents of `.env` and makes the environment variables available for other buildpacks executed after it (build) as well as setting runtime (launch) environment variables.

If any existing environment variable with the same name are present the values will be overwritten (override).

## What is a buildpack?

Buildpacks take in an application's code and prepare it to run, usually in production. Unlike a declarative system like Dockerfile, buildpacks are designed to be low or no configuration required. They can inspect the application and make dynamic adjustments using common community standards.

This buildpack is a Cloud Native Buildpack (CNB). CNB is a specification for building [OCI images](https://opencontainers.org/) (like docker). Some services natively support CNBs while other services might require you to generate an image locally and deploy that image through a container registry service.

You can find out more about the CNB project at https://buildpacks.io/.

## Develop

This buildpack is written in Rust using https://github.com/heroku/libcnb.rs. See that project for details.

To build this you'll need to [install Rust](https://www.rust-lang.org/tools/install). You'll also want install the [pack CLI](https://buildpacks.io/docs/tools/pack/cli/pack/), `brew install pack` on a mac. You'll also need [docker](https://docs.docker.com/engine/install/).

To develop:

```
$ cargo test
```

It can be useful to run your tests in a loop when developing:

```
$ cargo watch -c -x test
```

To run (slow) integration tests:

```
$ RUST_BACKTRACE=1 cargo test --all-features -- --include-ignored
```

To format code (if your IDE does not do it automatically on save):

```
$ cargo fmt --all
```

To lint:

```
$ cargo clippy --all-targets --all-features -- --deny warnings
```

## Package

```
$ cargo install libcnb-cargo
$ rustup target add x86_64-unknown-linux-musl
$ cargo libcnb package
```

This will generate a compiled binary in `target/buildpack/debug/dotenv` you can use compiled buildpack along with the `pack` CLI to run run against an application. When you do that you'll generate a runable docker image.

```
$ pack build my-image --buildpack target/buildpack/debug/dotenv --path tests/fixtures/hello_world
```

Once you've built the image you can access it via docker:

```
$ docker run -it --rm --entrypoint='/cnb/lifecycle/launcher' my-image bash
```

You can execute the default process type by running:

```
$ docker run -it --rm --env PORT=9292 -p 9292:9292 my-image
```
