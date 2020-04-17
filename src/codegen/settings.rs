#[derive(PartialEq, Clone, Copy)]
pub enum OptLevel {
    None,
    Speed,
    SpeedAndSize,
}

pub struct Settings {
    pub opt_level: OptLevel,
}

impl Settings {
    pub fn default() -> Self {
        Settings {
            opt_level: OptLevel::None,
        }
    }
}
