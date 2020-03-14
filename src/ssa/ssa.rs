#[derive(Debug, Clone)]
pub struct Register(u32);

#[derive(Debug, Clone)]
pub struct Op {
    pub dest: Register,
    pub typed: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Register(Register),
    Add(Register, Register),
    Sub(Register, Register),
    Mul(Register, Register),
    Div(Register, Register),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub block: Vec<Op>,
}