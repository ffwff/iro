use std::collections::HashMap;
use std::rc::Rc;
use crate::arch::context::*;
use crate::ssa::isa::FunctionName;

#[derive(Debug, Clone)]
pub struct Mmap {
    contents: *const u8,
    size: usize,
    context_locs: HashMap<Rc<FunctionName>, usize>,
}

#[derive(Debug, Clone)]
pub enum Error {
    UnableToMmap,
}

const PAGE_SIZE: usize = 4096;

fn round_to_page_size(n: usize) -> usize {
    (n + PAGE_SIZE) - (n % PAGE_SIZE)
}

impl Mmap {
    pub unsafe fn from_contexts(contexts: &Contexts) -> Result<Self, Error> {
        let mut size = 0usize;
        for (_, context) in contexts {
            size += context.code.len();
        }
        size = round_to_page_size(size);

        let (bytes, err) = {
            let mut bytes: *mut libc::c_void = std::ptr::null_mut();
            let err = libc::posix_memalign(&mut bytes, PAGE_SIZE, size);
            (bytes as *mut u8, err)
        };
        if err != 0 {
            return Err(Error::UnableToMmap);
        }

        let mut context_locs = HashMap::new();
        let mut placement = 0usize;
        for (name, context) in contexts {
            std::intrinsics::copy_nonoverlapping(context.code.as_ptr(),
                bytes.add(placement), context.code.len());
            context_locs.insert(name.clone(), placement);
            placement += context.code.len();
        }
        for (name, context) in contexts {
            let cur_context_loc = *context_locs.get(name).unwrap();
            for (label, func_name) in &context.func_relocation {
                let mut dist =
                    *context_locs.get(func_name).unwrap() as i64 -
                    (cur_context_loc as i64 + *label as i64);
                if dist < 0 {
                    dist = 0x1_0000_0000i64 + dist;
                }
                dist -= 4i64;
                let le_bytes = (dist as u32).to_le_bytes();
                for (i, byte) in le_bytes.iter().enumerate() {
                    bytes.add(cur_context_loc + label + i).write_unaligned(*byte);
                }
            }
        }
        libc::mprotect(bytes as *mut libc::c_void, size, libc::PROT_EXEC | libc::PROT_READ);

        Ok(Mmap {
            contents: bytes as *const u8,
            size,
            context_locs
        })
    }

    pub unsafe fn execute(&self) -> i64 {
        let main_name = Rc::new(FunctionName {
            name: Rc::from("main"),
            arg_types: vec![],
        });
        let main_offset = self.context_locs[&main_name];
        let func = std::mem::transmute::<_, (extern "C" fn() -> i64)>(self.contents.add(main_offset));
        func()
    }   
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.contents as *mut libc::c_void, self.size);
        }
    }
}