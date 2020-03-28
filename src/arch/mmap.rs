use crate::ssa::isa::FunctionName;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Mmap {
    pub(crate) contents: *const u8,
    pub(crate) size: usize,
    pub(crate) context_locs: HashMap<Rc<FunctionName>, *const libc::c_void>,
}

impl Mmap {
    pub unsafe fn execute(&self, function: &Rc<FunctionName>) -> Result<(), ()> {
        if let Some(ptr) = self.context_locs.get(function) {
            dbg_println!("execute {:p}", *ptr);
            let func = std::mem::transmute::<*const libc::c_void, (extern "sysv64" fn())>(*ptr);
            Ok(func())
        } else {
            Err(())
        }
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.contents as *mut libc::c_void, self.size);
        }
    }
}
