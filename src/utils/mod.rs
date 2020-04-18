pub mod optcell;
pub mod pipeline;
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
