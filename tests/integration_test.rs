use libcnb_test::{assert_contains, BuildConfig, BuildpackReference, TestRunner};

#[test]
#[ignore = "integration test"]
fn test_getting_started_app() {
    TestRunner::default().build(
        BuildConfig::new("heroku/builder:22", "tests/fixtures/hello_world")
            .buildpacks(vec![BuildpackReference::Crate]),
        |context| {
            println!("{}", context.pack_stdout);
            assert_contains!(
                context.pack_stdout,
                "export WEB_CONCURRENCY=<1 character(s)>"
            );
        },
    );
}
