use iro::arch::x86_64::encoder::encode_instruction;
use iro::arch::x86_64::isa::*;
use crate::arch::utils;
#[cfg(test)]

#[test]
fn mov32_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Register(Reg::Rbx),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "mov eax,ebx\n");
}

#[test]
fn mov64_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Register(Reg::Rbx),
        size: OperandSize::I64,
    }));
    assert_eq!(utils::objdump(&context.code), "mov rax,rbx\n");
}

#[test]
fn mov32_reg_imm() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::U32(0x1337),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "mov eax,0x1337\n");
}

#[test]
fn mov32_reg_mem() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Memory {
            base: Reg::Rax,
            disp: -0xA
        },
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "mov eax,DWORD PTR [rax-0xa]\n");
}

#[test]
fn mov32_mem_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Memory {
            base: Reg::Rax,
            disp: -0xA
        },
        src: Operand::Register(Reg::Rax),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "mov DWORD PTR [rax-0xa],eax\n");
}

#[test]
fn mov32_r12_r8() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::R12),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "mov r12d,r8d\n");
}

#[test]
fn mov64_r12_r8() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::R12),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I64,
    }));
    assert_eq!(utils::objdump(&context.code), "mov r12,r8\n");
}

#[test]
fn add32_reg_imm8() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x13),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "add ecx,0x13\n");
}

#[test]
fn add32_reg_imm32() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x1337),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "add ecx,0x1337\n");
}

#[test]
fn add32_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "add ecx,r8d\n");
}

#[test]
fn add64_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I64,
    }));
    assert_eq!(utils::objdump(&context.code), "add rcx,r8\n");
}

#[test]
fn sub32_reg_imm8() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x13),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "sub ecx,0x13\n");
}

#[test]
fn sub32_reg_imm32() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x1337),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "sub ecx,0x1337\n");
}

#[test]
fn sub32_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I32,
    }));
    assert_eq!(utils::objdump(&context.code), "sub ecx,r8d\n");
}

#[test]
fn sub64_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I64,
    }));
    assert_eq!(utils::objdump(&context.code), "sub rcx,r8\n");
}

#[test]
fn cmp32_reg_imm8() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Cmp {
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::U32(0x13),
            size: OperandSize::I32,
        },
        is_postlude: false,
    });
    assert_eq!(utils::objdump(&context.code), "cmp ecx,0x13\n");
}

#[test]
fn cmp32_reg_imm32() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Cmp{
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::U32(0x1337),
            size: OperandSize::I32,
        },
        is_postlude: false,
    });
    assert_eq!(utils::objdump(&context.code), "cmp ecx,0x1337\n");
}

#[test]
fn cmp32_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Cmp{
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::Register(Reg::R8),
            size: OperandSize::I32,
        },
        is_postlude: false,
    });
    assert_eq!(utils::objdump(&context.code), "cmp ecx,r8d\n");
}

#[test]
fn cmp64_reg_reg() {
    let mut context = utils::context();
    encode_instruction(&mut context, &Ins::Cmp{
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::Register(Reg::R8),
            size: OperandSize::I64,
        },
        is_postlude: false,
    });
    assert_eq!(utils::objdump(&context.code), "cmp rcx,r8\n");
}