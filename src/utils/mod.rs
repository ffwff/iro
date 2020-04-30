pub mod optcell;
#[macro_use]
pub mod overlay;
pub mod uniquerc;

#[macro_use]
macro_rules! dbg_println {
    ($first:expr) => {{
        if cfg!(debug_assertions) {
            eprint!("\x1b[1m\x1b[38;5;11m{}:{}:{}:\x1b[0m ", file!(), line!(), column!());
            eprintln!($first)
        }
    }};
    ($first:expr, $($last:expr),+) => {{
        if cfg!(debug_assertions) {
            eprint!("\x1b[1m\x1b[38;5;11m{}:{}:{}:\x1b[0m ", file!(), line!(), column!());
            eprintln!($first, $($last),+)
        }
    }};
}

#[macro_use]
macro_rules! fnv_hashmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(fnv_hashmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { fnv_hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = fnv_hashmap!(@count $($key),*);
            let mut _map = ::fnv::FnvHashMap::with_capacity_and_hasher(_cap, Default::default());
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}