use std::collections::HashMap;
use crate::arch::context::*;

pub struct Mmap {
    contents: *mut u8,
    execute_start: *mut u8,
}

const PAGE_SIZE: usize = 4096;

fn round_to_page_size(n: usize) -> usize {
    (n + PAGE_SIZE) - (n % PAGE_SIZE)
}

impl Mmap {
    pub unsafe fn from_contexts(contexts: &Contexts) -> Self {
        let mut size = 0usize;
        for (name, context) in contexts {
            size += context.code.len();
        }
        size = round_to_page_size(size);

        let (bytes, err) = {
            let mut bytes: *mut libc::c_void = std::ptr::null_mut();
            let err = libc::posix_memalign(&mut bytes, PAGE_SIZE, size);
            (bytes as *mut u8, err)
        };
        if err != 0 {
            println!("unable to can: {:?}", err);
            panic!("///");
        }
        println!("bytes: {:?} {}", bytes, size);

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
                for i in 0..4 {
                    bytes.add(cur_context_loc + label + i).write_unaligned(le_bytes[i]);
                }
            }
        }
        libc::abort();
    }
}