use crate::codegen::settings::Settings;
use crate::compiler;
use crate::runtime::Runtime;
use crate::ssa::isa;

use std::borrow::Borrow;

pub trait JitBackend {
    unsafe fn run(
        &self,
        program: &isa::Program,
        settings: &Settings,
        runtime: &Runtime,
    ) -> Result<(), compiler::Error>;
}

pub trait ObjectBackend {
    fn generate_object(
        &self,
        program: &isa::Program,
        settings: &Settings,
    ) -> Result<Vec<u8>, compiler::Error>;
}

pub struct Backend {
    pub jit_backend: Option<Box<dyn JitBackend>>,
    pub object_backend: Option<Box<dyn ObjectBackend>>,
}

impl Backend {
    pub fn with_jit<T: 'static + JitBackend>(backend: T) -> Self {
        Self {
            jit_backend: Some(Box::new(backend)),
            object_backend: None,
        }
    }

    pub fn with_object<T: 'static + ObjectBackend>(backend: T) -> Self {
        Self {
            jit_backend: None,
            object_backend: Some(Box::new(backend)),
        }
    }

    pub fn with_jit_and_object<T: 'static + JitBackend + ObjectBackend + Clone>(
        backend: T,
    ) -> Self {
        Self {
            jit_backend: Some(Box::new(backend.clone())),
            object_backend: Some(Box::new(backend)),
        }
    }

    pub fn as_jit_backend(&self) -> Option<&dyn JitBackend> {
        self.jit_backend.as_ref().map(|opt| opt.borrow())
    }

    pub fn as_object_backend(&self) -> Option<&dyn ObjectBackend> {
        self.object_backend.as_ref().map(|opt| opt.borrow())
    }
}
