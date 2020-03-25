use crate::arch::context;
use crate::arch::x86_64::isa;
use isa::{InsType, Operand};

use crate::arch::context::RelativeRelocation;

pub fn encode_blocks(blocks: &Vec<isa::Block>) -> context::Context {
    let mut context = context::Context {
        code: vec![],
        data: vec![],
        func_relocation: vec![],
        rel_relocation: vec![],
    };
    let mut label_locations = vec![];
    for block in blocks {
        dbg_println!("encoding block: {:#?}", block);
        if context.code.is_empty() {
            label_locations.push(0);
        } else {
            label_locations.push(context.code.len() - 1);
        }
        for ins in &block.ins {
            encode_instruction(&mut context, ins);
        }
    }
    for relocation in &context.rel_relocation {
        let distance = label_locations[relocation.branch] as isize - relocation.label as isize - 3;
        let bytes = if distance < 0 {
            ((0x1_0000_0000 + distance) as u32).to_le_bytes()
        } else {
            (distance as u32).to_le_bytes()
        };
        for (i, byte) in bytes.iter().enumerate() {
            context.code[relocation.label + i] = *byte;
        }
    }
    if !context.code.is_empty() {
        let len = ((context.code.len() - 1) | 15) + 1;
        context.code.resize(len, 0x90);
    }
    context
}

fn encode_instruction(dest: &mut context::Context, ins: &isa::Ins) {
    dbg_println!("encoding insn: {:#?}", ins);
    match &ins.typed {
        InsType::MovI64(_ops) => unimplemented!(),
        InsType::MovI32(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(left), Operand::U32(n)) => {
                assert!((*left as u8) <= 0b111);
                dest.code.push(0xB8 + *left as u8);
                dest.code.extend_from_slice(&n.to_le_bytes());
            }
            (Operand::Memory { .. }, Operand::Register(_)) => {
                dbg_println!("mem<-reg");
                rex_prefix(dest, ops.src.clone(), ops.dest.clone());
                dest.code.push(0x89);
                modrm(dest, ops.src.clone(), ops.dest.clone(), 0);
            }
            (_, _) => {
                rex_prefix(dest, ops.dest.clone(), ops.src.clone());
                dest.code.push(0x8B);
                modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
            }
        },
        InsType::AddI32(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(left), Operand::U32(n)) => {
                assert!((*left as u8) <= 0b111);
                if *n <= 0xFF {
                    dest.code.push(0x83);
                    modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
                    dest.code.push(*n as u8);
                } else {
                    dest.code.push(0x81);
                    modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
                    dest.code.extend_from_slice(&n.to_le_bytes());
                }
            }
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => {
                rex_prefix(dest, ops.src.clone(), ops.dest.clone());
                dest.code.push(0x01);
                modrm(dest, ops.src.clone(), ops.dest.clone(), 0);
            }
        },
        InsType::SubI32(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(left), Operand::U32(n)) => {
                assert!((*left as u8) <= 0b111);
                if *n <= 0xFF {
                    dest.code.push(0x83);
                    modrm(dest, ops.dest.clone(), ops.src.clone(), 0b101);
                    dest.code.push(*n as u8);
                } else {
                    dest.code.push(0x81);
                    modrm(dest, ops.dest.clone(), ops.src.clone(), 0b101);
                    dest.code.extend_from_slice(&n.to_le_bytes());
                }
            }
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => {
                unimplemented!()
                // dest.code.push(0x29);
                // modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
            }
        },
        InsType::MulI32(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(_left), Operand::U32(_n)) => {
                unimplemented!();
            }
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => unimplemented!(),
        },
        InsType::DivI32(_ops) => unimplemented!(),
        InsType::CmpI32 { ops, .. } => match (&ops.dest, &ops.src) {
            (Operand::Register(left), Operand::U32(n)) => {
                assert!((*left as u8) <= 0b111);
                if *n <= 0xFF {
                    dest.code.push(0x83);
                    modrm(dest, ops.dest.clone(), ops.src.clone(), 0b111);
                    dest.code.push(*n as u8);
                } else {
                    dest.code.push(0x81);
                    modrm(dest, ops.dest.clone(), ops.src.clone(), 0b111);
                    dest.code.extend_from_slice(&n.to_le_bytes());
                }
            }
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => {
                dest.code.push(0x39);
                modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
            }
        },
        InsType::Jmp(branch) => {
            dest.code.push(0xE9);
            let len = dest.code.len();
            dest.code.push(0x0);
            dest.code.push(0x0);
            dest.code.push(0x0);
            dest.code.push(0x0);
            dest.rel_relocation.push(RelativeRelocation {
                label: len,
                branch: *branch,
            });
        }
        InsType::Jgt(branch)
        | InsType::Jlt(branch)
        | InsType::Jge(branch)
        | InsType::Jle(branch) => {
            dest.code.push(0x0F);
            match &ins.typed {
                InsType::Jgt(_) => dest.code.push(0x8F),
                InsType::Jlt(_) => dest.code.push(0x8C),
                InsType::Jge(_) => dest.code.push(0x8D),
                InsType::Jle(_) => dest.code.push(0x8E),
                _ => unreachable!(),
            }
            let len = dest.code.len();
            dest.code.push(0x0);
            dest.code.push(0x0);
            dest.code.push(0x0);
            dest.code.push(0x0);
            dest.rel_relocation.push(RelativeRelocation {
                label: len,
                branch: *branch,
            });
        }
        InsType::Call(name) => {
            dest.code.push(0xE8);
            let len = dest.code.len();
            for _ in 0..4 {
                dest.code.push(0);
            }
            dest.code.push(0x90); // this nop is a HACK to make relocation easier
            dest.func_relocation.push((len, name.clone()));
        }
        InsType::Ret => {
            dest.code.push(0xC3);
        }
        InsType::Push(_op) => unimplemented!(),
        InsType::Enter {
            local_size,
            save_regs,
        } => {
            // push all the save regs
            for &reg in save_regs.iter() {
                if (reg as u8) <= 0b111 {
                    dest.code.push(0x50 + (reg as u8));
                } else {
                    dest.code.push(0x41);
                    dest.code.push(0x50 + (reg as u8) - 0b1000);
                }
            }
            // push rbp
            dest.code.push(0x55);
            // mov rbp <- rsp
            dest.code.push(0x48);
            dest.code.push(0x89);
            dest.code.push(0xE5);
            let local_size = *local_size;
            if local_size != 0 {
                // sub rsp, alloc
                if local_size <= 0xFF {
                    dest.code.push(0x48);
                    dest.code.push(0x83);
                    dest.code.push(0xEC);
                    dest.code.push(local_size as u8);
                } else {
                    dest.code.push(0x48);
                    dest.code.push(0x81);
                    dest.code.push(0xEC);
                    dest.code.extend_from_slice(&local_size.to_le_bytes());
                }
            }
        }
        InsType::LeaveAndRet { save_regs } => {
            // leave
            dest.code.push(0xC9);
            // pop all the save regs
            for &reg in save_regs.iter().rev() {
                if (reg as u8) <= 0b111 {
                    dest.code.push(0x58 + (reg as u8));
                } else {
                    dest.code.push(0x41);
                    dest.code.push(0x58 + (reg as u8) - 0b1000);
                }
            }
            // ret
            dest.code.push(0xC3);
        }
        _ => unreachable!(),
    }
    dbg_println!(" => {:02x?}", dest.code);
}

fn modrm_normalize_reg(reg: u8) -> u8 {
    if reg > 0b111 {
        reg - 0b1000
    } else {
        reg
    }
}

fn rex_prefix(dest: &mut context::Context, odest: Operand, osrc: Operand) {
    match (odest, osrc) {
        (Operand::Register(left), Operand::Register(right))
        | (
            Operand::Register(left),
            Operand::Memory {
                disp: _,
                base: right,
            },
        ) => {
            if (left as u8) > 0b111 || (right as u8) > 0b111 {
                let mut rex_form = 0b0100_0000;
                if (left as u8) > 0b111 {
                    rex_form |= 0b100;
                }
                if (right as u8) > 0b111 {
                    rex_form |= 0b001;
                }
                dest.code.push(rex_form);
            }
        }
        (_, _) => (),
    }
}

fn modrm(dest: &mut context::Context, odest: Operand, osrc: Operand, extension: u8) {
    match (odest, osrc) {
        (Operand::Register(left), Operand::Register(right)) => dest.code.push(
            0b11_000_000
                | (modrm_normalize_reg(left as u8) << 3)
                | modrm_normalize_reg(right as u8),
        ),
        (Operand::Register(reg), Operand::Memory { disp, base }) => {
            if -127 <= disp && disp <= 127 {
                dest.code.push(
                    0b01_000_000
                        | (modrm_normalize_reg(reg as u8) << 3)
                        | modrm_normalize_reg(base as u8),
                );
                if disp < 0 {
                    dest.code.push((0x100 + disp) as u8);
                } else {
                    dest.code.push(disp as u8);
                }
            } else {
                dest.code.push(
                    0b10_000_000
                        | (modrm_normalize_reg(reg as u8) << 3)
                        | modrm_normalize_reg(base as u8),
                );
                if disp < 0 {
                    dest.code
                        .extend_from_slice(&(0x1_0000_0000i64 - (disp as i64)).to_le_bytes());
                } else {
                    dest.code.extend_from_slice(&disp.to_le_bytes());
                }
            }
        }
        (Operand::Register(left), right) if right.is_lit() => dest
            .code
            .push(0b11_000_000 | (extension << 3) | modrm_normalize_reg(left as u8)),
        _ => unimplemented!(),
    }
}
