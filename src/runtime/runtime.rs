use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq)]
pub struct Runtime {
    funcs: HashMap<String, GenericFunction>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { funcs: hashmap![] }
    }

    pub fn funcs(&self) -> &HashMap<String, GenericFunction> {
        &self.funcs
    }

    pub fn insert_func<S, F>(&mut self, string: S, generic: F)
    where
        S: ToString,
        F: ToGenericFunction,
    {
        self.funcs.insert(string.to_string(), generic.to_generic());
    }

    pub fn remove_func<Q: ?Sized>(&mut self, string: &Q) -> Option<GenericFunction>
    where
        String: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.funcs.remove(string)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct GenericFunction(pub(self) *const libc::c_void);
unsafe impl Send for GenericFunction {}

impl GenericFunction {
    pub fn ptr(&self) -> *const libc::c_void {
        self.0
    }
}

pub trait ToGenericFunction {
    fn to_generic(&self) -> GenericFunction;
}

// Impls for function pointers
macro_rules! fnptr_impls_safety_abi {
    ($FnTy: ty, $($Arg: ident),*) => {
        impl<Ret, $($Arg),*> ToGenericFunction for $FnTy {
            fn to_generic(&self) -> GenericFunction {
                GenericFunction(*self as _)
            }
        }
    }
}

macro_rules! fnptr_impls_args {
    ($($Arg: ident),+) => {
        fnptr_impls_safety_abi! { extern "C" fn($($Arg),+) -> Ret, $($Arg),+ }
        fnptr_impls_safety_abi! { unsafe extern "C" fn($($Arg),+) -> Ret, $($Arg),+ }
    };
    () => {
        // No variadic functions with 0 parameters
        fnptr_impls_safety_abi! { extern "C" fn() -> Ret, }
        fnptr_impls_safety_abi! { unsafe extern "C" fn() -> Ret, }
    };
}

fnptr_impls_args! {}
fnptr_impls_args! { A }
fnptr_impls_args! { A, B }
fnptr_impls_args! { A, B, C }
fnptr_impls_args! { A, B, C, D }
fnptr_impls_args! { A, B, C, D, E }
fnptr_impls_args! { A, B, C, D, E, F }
fnptr_impls_args! { A, B, C, D, E, F, G }
fnptr_impls_args! { A, B, C, D, E, F, G, H }
fnptr_impls_args! { A, B, C, D, E, F, G, H, I }
fnptr_impls_args! { A, B, C, D, E, F, G, H, I, J }
fnptr_impls_args! { A, B, C, D, E, F, G, H, I, J, K }
fnptr_impls_args! { A, B, C, D, E, F, G, H, I, J, K, L }
