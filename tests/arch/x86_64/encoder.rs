use iro::arch::x86_64::encoder::encode_instruction;
use iro::arch::x86_64::isa::*;
use crate::arch::x86_64::utils::*;
use crate::arch::utils::context;

#[cfg(test)]

#[test]
fn mov32_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Register(Reg::Rbx),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "mov eax,ebx\n");
}

#[test]
fn mov32_reg_imm() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::U32(0x1337),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "mov eax,0x1337\n");
}

#[test]
fn mov32_reg_mem() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Memory {
            base: Reg::Rax,
            disp: -0xA
        },
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "mov eax,DWORD PTR [rax-0xa]\n");
}

#[test]
fn mov32_reg_mem32() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Memory {
            base: Reg::Rax,
            disp: 0x12345
        },
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "mov eax,DWORD PTR [rax+0x12345]\n");
}

#[test]
fn mov32_mem_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Memory {
            base: Reg::Rax,
            disp: -0xA
        },
        src: Operand::Register(Reg::Rax),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "mov DWORD PTR [rax-0xa],eax\n");
}

#[test]
fn mov32_r12_r8() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::R12),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "mov r12d,r8d\n");
}

#[test]
fn mov64_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::Register(Reg::Rbx),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "mov rax,rbx\n");
}

#[test]
fn mov64_reg_imm() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::Rax),
        src: Operand::U64(0x12345678),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "movabs rax,0x12345678\n");
}

#[test]
fn mov64_r12_r8() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Mov(TwoOperands {
        dest: Operand::Register(Reg::R12),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "mov r12,r8\n");
}

#[test]
fn add32_reg_imm8() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x13),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "add ecx,0x13\n");
}

#[test]
fn add32_reg_imm32() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x1337),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "add ecx,0x1337\n");
}

#[test]
fn add32_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "add ecx,r8d\n");
}

#[test]
fn add64_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "add rcx,r8\n");
}

#[test]
fn add64_reg_imm32() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Add(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x1337),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "add rcx,0x1337\n");
}

#[test]
fn sub32_reg_imm8() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x13),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "sub ecx,0x13\n");
}

#[test]
fn sub32_reg_imm32() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x1337),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "sub ecx,0x1337\n");
}

#[test]
fn sub32_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I32,
    }));
    assert_eq!(objdump(&context.code), "sub ecx,r8d\n");
}

#[test]
fn sub64_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::Register(Reg::R8),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "sub rcx,r8\n");
}

#[test]
fn sub64_reg_imm32() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Sub(TwoOperands {
        dest: Operand::Register(Reg::Rcx),
        src: Operand::U32(0x1337),
        size: OperandSize::I64,
    }));
    assert_eq!(objdump(&context.code), "sub rcx,0x1337\n");
}

#[test]
fn cmp32_reg_imm8() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Cmp {
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::U32(0x13),
            size: OperandSize::I32,
        },
        is_postlude: false,
    });
    assert_eq!(objdump(&context.code), "cmp ecx,0x13\n");
}

#[test]
fn cmp32_reg_imm32() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Cmp{
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::U32(0x1337),
            size: OperandSize::I32,
        },
        is_postlude: false,
    });
    assert_eq!(objdump(&context.code), "cmp ecx,0x1337\n");
}

#[test]
fn cmp32_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Cmp{
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::Register(Reg::R8),
            size: OperandSize::I32,
        },
        is_postlude: false,
    });
    assert_eq!(objdump(&context.code), "cmp ecx,r8d\n");
}

#[test]
fn cmp64_reg_reg() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Cmp{
        ops: TwoOperands {
            dest: Operand::Register(Reg::Rcx),
            src: Operand::Register(Reg::R8),
            size: OperandSize::I64,
        },
        is_postlude: false,
    });
    assert_eq!(objdump(&context.code), "cmp rcx,r8\n");
}

#[test]
fn jmp() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Jmp(0));
    assert_eq!(objdump(&context.code), "jmp 0x5\n");
}

#[test]
fn jlt() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Jlt(0));
    assert_eq!(objdump(&context.code), "jl 0x6\n");
}

#[test]
fn jgt() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Jgt(0));
    assert_eq!(objdump(&context.code), "jg 0x6\n");
}

#[test]
fn jge() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Jge(0));
    assert_eq!(objdump(&context.code), "jge 0x6\n");
}

#[test]
fn jle() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Jle(0));
    assert_eq!(objdump(&context.code), "jle 0x6\n");
}

#[test]
fn call() {
    use std::rc::Rc;
    use iro::ssa::isa::FunctionName;
    let mut context = context();
    encode_instruction(&mut context, &Ins::Call(Rc::new(FunctionName {
        name: Rc::from(""),
        arg_types: vec![]
    })));
    assert_eq!(objdump(&context.code), "call 0x5\n");
}

#[test]
fn enter() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Enter { local_size: 0x10, save_regs: vec![] });
    assert_eq!(objdump_all(&context.code), "push rbp\nmov rbp,rsp\nsub rsp,0x10\n");
}

#[test]
fn enter_with_save_regs() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::Enter { local_size: 0x10, save_regs: vec![ Reg::Rbx, Reg::R10 ] });
    assert_eq!(objdump_all(&context.code), "push rbx\npush r10\npush rbp\nmov rbp,rsp\nsub rsp,0x10\n");
}

#[test]
fn leave_with_save_regs() {
    let mut context = context();
    encode_instruction(&mut context, &Ins::LeaveAndRet { save_regs: vec![ Reg::Rbx, Reg::R10 ] });
    assert_eq!(objdump_all(&context.code), "leave \npop r10\npop rbx\nret \n");
}