## Dotenv Buildpack

TODO, high-level description of this buildpack

## Detect

If `.env` is present it will also require `dotenv` to run itself (see build below). This buildpack alwasys provides `dotenv`.

## Build

TODO, detailed description of the logic this buildpack uses to achieve it's high level goal.

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
$ pack build my-image  --buildpack target/buildpack/debug/dotenv --path tests/fixtures/hello_world
```

Once you've built the image you can access it via docker:

```
$ docker run -it --rm --entrypoint='/cnb/lifecycle/launcher' my-image bash
```

You can execute the default process type by running:

```
$ docker run -it --rm --env PORT=9292 -p 9292:9292 my-image
```