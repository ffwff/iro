use crate::arch::context::*;
use crate::ssa::isa::FunctionName;
use std::collections::HashMap;
use std::io::Error;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Mmap {
    contents: *const u8,
    size: usize,
    context_locs: HashMap<Rc<FunctionName>, *const libc::c_void>,
}

const PAGE_SIZE: usize = 4096;

fn round_to_page_size(n: usize) -> usize {
    (n + PAGE_SIZE) - (n % PAGE_SIZE)
}

impl Mmap {
    pub unsafe fn from_contexts(contexts: &Contexts) -> Result<Self, Error> {
        let mut size = 0usize;
        for (_, function) in contexts {
            match function {
                Function::Context(context) => {
                    size += context.code.len();
                }
                _ => (),
            }
        }
        size = round_to_page_size(size);

        let (bytes, err) = {
            let mut bytes: *mut libc::c_void = std::ptr::null_mut();
            let err = libc::posix_memalign(&mut bytes, PAGE_SIZE, size);
            (bytes as *mut u8, err)
        };
        if err != 0 {
            return Err(Error::from_raw_os_error(err));
        }
        dbg_println!("bytes: {:p}", bytes);

        let mut context_locs = HashMap::new();
        let mut placement = 0usize;
        for (name, function) in contexts {
            match function {
                Function::Context(context) => {
                    std::intrinsics::copy_nonoverlapping(
                        context.code.as_ptr(),
                        bytes.add(placement),
                        context.code.len(),
                    );
                    let context_loc = bytes.add(placement);
                    dbg_println!("function {:#?} => {:p}", name, context_loc);
                    context_locs.insert(name.clone(), context_loc as _);
                    placement += context.code.len();
                }
                Function::Extern(generic) => {
                    context_locs.insert(name.clone(), generic.0);
                }
            }
        }
        for (name, function) in contexts {
            match function {
                Function::Context(context) => {
                    let cur_context_loc = *context_locs.get(name).unwrap();
                    for (label, func_name) in &context.func_relocation {
                        // FIXME: this is architecture dependent
                        let mut dist = *context_locs.get(func_name).unwrap() as i64
                            - (cur_context_loc as i64 + *label as i64);
                        if dist < 0 {
                            dist = 0x1_0000_0000i64 + dist;
                        }
                        dist -= 4i64;
                        let le_bytes = (dist as u32).to_le_bytes();
                        for (i, byte) in le_bytes.iter().enumerate() {
                            (cur_context_loc as *mut u8)
                                .add(label + i)
                                .write_unaligned(*byte);
                        }
                    }
                }
                _ => (),
            }
        }
        libc::mprotect(
            bytes as *mut libc::c_void,
            size,
            libc::PROT_EXEC | libc::PROT_READ,
        );

        Ok(Mmap {
            contents: bytes as *const u8,
            size,
            context_locs,
        })
    }

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
