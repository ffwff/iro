use std::path::PathBuf;

#[derive(PartialEq, Clone, Copy)]
pub enum OptLevel {
    None,
    Speed,
    SpeedAndSize,
}

pub struct Settings {
    pub opt_level: OptLevel,
    pub prelude: Option<PathBuf>,
}

impl Settings {
    pub fn default() -> Self {
        Settings {
            opt_level: OptLevel::None,
            prelude: None,
        }
    }
}
