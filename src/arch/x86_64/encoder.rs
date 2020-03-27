use crate::arch::context;
use crate::arch::x86_64::isa;
use isa::{Ins, Operand, OperandSize, TwoOperands};

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
    context
}

fn encode_instruction(dest: &mut context::Context, ins: &isa::Ins) {
    dbg_println!("encoding insn: {:#?}", ins);
    match &ins {
        Ins::InlineBytes(bytes) => {
            dest.code.extend_from_slice(bytes.as_slice());
        }
        Ins::Mov(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(left), Operand::U32(n)) => {
                rex_prefix(dest, ops, true, false);
                dest.code.push(0xB8 + *left as u8);
                dest.code.extend_from_slice(&n.to_le_bytes());
            }
            (Operand::Register(left), Operand::U64(n)) => {
                rex_prefix(dest, ops, true, false);
                dest.code.push(0xB8 + *left as u8);
                dest.code.extend_from_slice(&n.to_le_bytes());
            }
            (Operand::Memory { .. }, Operand::Register(_)) => {
                rex_prefix(dest, ops, false, false);
                dest.code.push(0x89);
                modrm(dest, ops.src.clone(), ops.dest.clone(), 0);
            }
            (_, _) => {
                rex_prefix(dest, ops, true, false);
                dest.code.push(0x8B);
                modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
            }
        },
        Ins::Add(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(_), Operand::U32(n)) => {
                rex_prefix(dest, ops, true, false);
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
            (Operand::Register(_), Operand::U64(_)) => unimplemented!(),
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => {
                rex_prefix(dest, ops, false, false);
                dest.code.push(0x01);
                modrm(dest, ops.src.clone(), ops.dest.clone(), 0);
            }
        },
        Ins::Sub(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(_), Operand::U32(n)) => {
                rex_prefix(dest, ops, true, false);
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
                rex_prefix(dest, ops, false, false);
                dest.code.push(0x29);
                modrm(dest, ops.src.clone(), ops.dest.clone(), 0);
            }
        },
        Ins::Mul(ops) => match (&ops.dest, &ops.src) {
            (Operand::Register(_left), Operand::U32(_n)) => {
                unimplemented!();
            }
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => unimplemented!(),
        },
        Ins::Div(_ops) => unimplemented!(),
        Ins::Cmp { ops, .. } => match (&ops.dest, &ops.src) {
            (Operand::Register(_), Operand::U32(n)) => {
                rex_prefix(dest, ops, true, false);
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
            (Operand::Register(_), Operand::U64(_)) => unimplemented!(),
            (Operand::Memory { .. }, Operand::Register(_))
            | (Operand::Register(_), Operand::Memory { .. }) => unimplemented!(),
            (_, _) => {
                rex_prefix(dest, ops, false, false);
                dest.code.push(0x39);
                modrm(dest, ops.dest.clone(), ops.src.clone(), 0);
            }
        },
        Ins::Jmp(branch) => {
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
        Ins::Jgt(branch) | Ins::Jlt(branch) | Ins::Jge(branch) | Ins::Jle(branch) => {
            dest.code.push(0x0F);
            match &ins {
                Ins::Jgt(_) => dest.code.push(0x8F),
                Ins::Jlt(_) => dest.code.push(0x8C),
                Ins::Jge(_) => dest.code.push(0x8D),
                Ins::Jle(_) => dest.code.push(0x8E),
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
        Ins::Call(name) => {
            dest.code.push(0xE8);
            let len = dest.code.len();
            for _ in 0..4 {
                dest.code.push(0);
            }
            dest.code.push(0x90); // this nop is a HACK to make relocation easier
            dest.func_relocation.push((len, name.clone()));
        }
        Ins::Ret => {
            dest.code.push(0xC3);
        }
        Ins::Push(_op) => unimplemented!(),
        Ins::Enter {
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
        Ins::LeaveAndRet { save_regs } => {
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

fn rex_prefix(dest: &mut context::Context, ops: &TwoOperands, dest_first: bool, rm_field: bool) {
    let mut rex_form = 0b0100_0000;
    let mut has_rex_form = false;
    match {
        if dest_first {
            (&ops.dest, &ops.src)
        } else {
            (&ops.src, &ops.dest)
        }
    } {
        (Operand::Register(left), Operand::U32(_)) => {
            if ops.size == OperandSize::I64 {
                rex_form |= 0b1000;
                has_rex_form = true;
            }
            if (*left as u8) > 0b111 {
                rex_form |= 0b100;
                has_rex_form = true;
            }
        }
        (Operand::Register(left), Operand::U64(_)) => {
            rex_form |= 0b1000;
            has_rex_form = true;
            if (*left as u8) > 0b111 {
                rex_form |= 0b001;
            }
            if rm_field {
                rex_form |= 0b100;
            }
        }
        (Operand::Register(left), Operand::Register(right))
        | (
            Operand::Register(left),
            Operand::Memory {
                disp: _,
                base: right,
            },
        ) => {
            if ops.size == OperandSize::I64 {
                rex_form |= 0b1000;
                has_rex_form = true;
            }
            if (*left as u8) > 0b111 || (*right as u8) > 0b111 {
                if (*left as u8) > 0b111 {
                    rex_form |= 0b100;
                }
                if (*right as u8) > 0b111 {
                    rex_form |= 0b001;
                }
                has_rex_form = true;
            }
        }
        (_, _) => (),
    }
    if has_rex_form {
        dest.code.push(rex_form);
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
