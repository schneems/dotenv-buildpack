use libcnb::build::BuildContext;
use libcnb::data::layer_content_metadata::LayerTypes;
use libcnb::generic::GenericMetadata;
use libcnb::layer::{Layer, LayerResultBuilder};
use libcnb::layer_env::{LayerEnv, ModificationBehavior, Scope};
use libcnb::Buildpack;
use std::ffi::OsString;
use std::marker::PhantomData;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct EnvOverrideLayer<B> {
    env: Vec<(OsString, OsString)>,
    buildpack: PhantomData<B>,
}

impl<B> EnvOverrideLayer<B> {
    pub fn new(env: Vec<(OsString, OsString)>) -> Self {
        Self {
            env,
            buildpack: PhantomData,
        }
    }
}
impl<B> Layer for EnvOverrideLayer<B>
where
    B: Buildpack,
{
    type Buildpack = B;
    type Metadata = GenericMetadata;

    fn types(&self) -> LayerTypes {
        LayerTypes {
            build: true,
            launch: true,
            cache: false,
        }
    }

    fn create(
        &self,
        _context: &BuildContext<Self::Buildpack>,
        _layer_path: &std::path::Path,
    ) -> Result<libcnb::layer::LayerResult<Self::Metadata>, <Self::Buildpack as Buildpack>::Error>
    {
        self.env
            .iter()
            .fold(
                LayerResultBuilder::new(GenericMetadata::default()),
                |builder, (key, value)| {
                    builder.env(LayerEnv::new().chainable_insert(
                        Scope::All,
                        ModificationBehavior::Override,
                        key.clone(),
                        value.clone(),
                    ))
                },
            )
            .build()
    }
}
