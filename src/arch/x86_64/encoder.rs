use crate::arch::context;
use crate::arch::x86_64::isa;
use isa::{InsType, Operand, Reg};
use crate::ssa::isa::FunctionName;

pub fn encode_blocks(blocks: &Vec<isa::Block>) -> context::Context {
    let mut context = context::Context {
        code: vec![],
        data: vec![],
        func_relocation: vec![],
        label_relocation: vec![],
    };
    for block in blocks {
        println!("encoding block: {:#?}", block);
        for ins in &block.ins {
            encode_instruction(&mut context, ins);
        }
    }
    context
}

fn encode_instruction(dest: &mut context::Context, ins: &isa::Ins) {
    println!("encoding insn: {:#?}", ins);
    match &ins.typed {
        InsType::MovI64(ops) => unimplemented!(),
        InsType::MovI32(ops) => {
            match (&ops.dest, &ops.src) {
                (Operand::Register(left), Operand::U32(n))
                    if (*left as u8) <= 0b111 => {
                    dest.code.push(0xB0 + *left as u8);
                    dest.code.extend_from_slice(&n.to_le_bytes());
                }
                (_, _) => {
                    dest.code.push(0x89);
                    modrm(dest, ops.dest.clone(), ops.src.clone());
                }
            }
        },
        InsType::AddI32(ops) => {
            match (&ops.dest, &ops.src) {
                (Operand::Register(left), Operand::U32(n))
                    if (*left as u8) <= 0b111 => {
                    dest.code.push(0x81);
                    dest.code.extend_from_slice(&n.to_le_bytes());
                }
                (_, _) => {
                    dest.code.push(0x01);
                    modrm(dest, ops.dest.clone(), ops.src.clone());
                }
            }
        },
        InsType::SubI32(ops) => {
            dest.code.push(0x29);
            modrm(dest, ops.dest.clone(), ops.src.clone());
        },
        InsType::MulI32(ops) => {
            dest.code.push(0x0F);
            dest.code.push(0xAF);
            modrm(dest, ops.dest.clone(), ops.src.clone());
        },
        InsType::DivI32(ops) => unimplemented!(),
        InsType::CmpI32(ops) => {
            dest.code.push(0x39);
            modrm(dest, ops.dest.clone(), ops.src.clone());
        },
        InsType::Jmp(ops) => unimplemented!(),
        InsType::Jgt(ops) => unimplemented!(),
        InsType::Jlt(ops) => unimplemented!(),
        InsType::Call(name) => {
            dest.code.push(0xE8);
            let len = dest.code.len() - 1;
            for _ in 0..4 {
                dest.code.push(0);
            }
            dest.func_relocation.push((len, name.clone()));
        },
        InsType::Ret => {
            dest.code.push(0xC3);
        },
        InsType::Push(op) => unimplemented!(),
        InsType::Enter => {
            // push ebp
            dest.code.push(0x55);
            // mov esp <- ebp
            dest.code.push(0x89);
            dest.code.push(0xEC);
        },
        InsType::LeaveAndRet => {
            // leave
            dest.code.push(0xC9);
            // ret
            dest.code.push(0xC3);
        },
        _ => unreachable!()
    }
    println!(" => {:02x?}", dest.code);
}

fn modrm(dest: &mut context::Context, odest: Operand, osrc: Operand) {
    match (odest, osrc) {
        (Operand::Register(left), Operand::Register(right))
            if (left as u8) <= 0b111 && (right as u8) <= 0b111
                => dest.code.push(0b11_000_000 | ((left as u8) << 3) | (right as u8)),
        _ => unimplemented!(),
    }
}