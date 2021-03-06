use crate::ssa::isa::*;

struct ConstPrinter<'a>(pub &'a Constant);

impl<'a> std::fmt::Display for ConstPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Constant::Bool(k) => write!(f, "Bool({})", *k),
            Constant::I32(k) => write!(f, "I32({})", *k),
            Constant::I64(k) => write!(f, "I64({})", *k),
            Constant::F64(k) => write!(f, "F64({})", f64::from_bits(*k)),
        }
    }
}

pub struct InsPrinter<'a>(pub &'a Ins);

impl<'a> std::fmt::Display for InsPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(retvar) = self.0.retvar() {
            write!(f, "v{} = ", retvar)?;
        }
        match &self.0.typed {
            InsType::LoadNil => write!(f, "nil"),
            InsType::Alloca => write!(f, "alloca"),
            InsType::AllocHeap => write!(f, "alloc_heap"),
            InsType::Drop(arg) => write!(f, "drop v{}", arg),
            InsType::DeallocHeap(arg) => write!(f, "dealloc_heap v{}", arg),
            InsType::Move(var) => write!(f, "move v{}", var),
            InsType::MarkMoved(var) => write!(f, "mark_moved v{}", var),
            InsType::Copy(var) => write!(f, "copy v{}", var),
            InsType::Borrow { var, modifier } => write!(
                f,
                "borrow.{} v{}",
                match modifier {
                    BorrowModifier::Immutable => "imm",
                    BorrowModifier::Mutable => "mut",
                    BorrowModifier::Unique => "uni",
                },
                var
            ),
            InsType::Load(var) => write!(f, "load v{}", var),
            InsType::Store { source, dest } => write!(f, "store v{} -> v{}", source, dest),
            InsType::LoadArg(arg) => write!(f, "load_arg {}", arg),
            InsType::LoadI32(n) => write!(f, "load.I32 {}", n),
            InsType::LoadI64(n) => write!(f, "load.I64 {}", n),
            InsType::LoadF64(n) => write!(f, "load.F64 {}", f64::from_bits(*n)),
            InsType::LoadBool(n) => write!(f, "load.Bool {}", n),
            InsType::LoadSubstring(n) => write!(f, "load.Substring {:?}", n),
            InsType::LoadSlice(n) => write!(f, "load.slice {:?}", n),
            InsType::LoadStruct => write!(f, "load.struct"),
            InsType::MemberReference {
                left,
                indices,
                modifier,
            } => write!(
                f,
                "{} member v{} [{}]",
                match modifier {
                    ReferenceModifier::Copy => "copy",
                    ReferenceModifier::Move => "move",
                    ReferenceModifier::Borrow(modifier) => match modifier {
                        BorrowModifier::Immutable => "borrow.imm",
                        BorrowModifier::Mutable => "borrow.mut",
                        _ => unreachable!(),
                    },
                },
                left,
                indices
                    .iter()
                    .map(|index| {
                        match index.var {
                            MemberExprIndexVar::StructIndex(n) => format!("%{} {}", n, index.typed),
                            MemberExprIndexVar::Variable(n) => format!("v{} {}", n, index.typed),
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            InsType::MemberReferenceStore {
                indices,
                modifier,
                right,
            } => write!(
                f,
                "member v{} [{}] = {} v{}",
                *self.0.memref_store_left().unwrap(),
                indices
                    .iter()
                    .map(|index| {
                        match index.var {
                            MemberExprIndexVar::StructIndex(n) => format!("%{} {}", n, index.typed),
                            MemberExprIndexVar::Variable(n) => format!("v{} {}", n, index.typed),
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", "),
                match modifier {
                    ReferenceModifier::Copy => "copy",
                    ReferenceModifier::Move => "move",
                    _ => "(invalid)",
                },
                right
            ),
            InsType::Phi { vars, defines: _ } => write!(
                f,
                "phi [{}]",
                vars.iter()
                    .map(|var| var.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            InsType::Call { name, args } => write!(
                f,
                "call %{}({})",
                *name,
                args.iter()
                    .map(|var| "v".to_string() + &var.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            InsType::Return(var) => write!(f, "return v{}", var),
            InsType::Trap(trap) => write!(f, "trap {:?}", trap),
            InsType::Exit => write!(f, "exit"),
            InsType::Add((x, y)) => write!(f, "v{} + v{}", x, y),
            InsType::Sub((x, y)) => write!(f, "v{} - v{}", x, y),
            InsType::Mul((x, y)) => write!(f, "v{} * v{}", x, y),
            InsType::Div((x, y)) => write!(f, "v{} / v{}", x, y),
            InsType::Mod((x, y)) => write!(f, "v{} % v{}", x, y),
            InsType::Lt((x, y)) => write!(f, "v{} < v{}", x, y),
            InsType::Gt((x, y)) => write!(f, "v{} > v{}", x, y),
            InsType::Lte((x, y)) => write!(f, "v{} <= v{}", x, y),
            InsType::Gte((x, y)) => write!(f, "v{} >= v{}", x, y),
            InsType::Equ((x, y)) => write!(f, "v{} == v{}", x, y),
            InsType::Neq((x, y)) => write!(f, "v{} != v{}", x, y),
            InsType::OpConst {
                register,
                constant,
                op,
                reg_left,
            } => {
                let op_str = match *op {
                    OpConst::Add => "+",
                    OpConst::Sub => "-",
                    OpConst::Mul => "*",
                    OpConst::Div => "/",
                    OpConst::Mod => "%",
                    OpConst::Lt => "<",
                    OpConst::Gt => ">",
                    OpConst::Lte => "<=",
                    OpConst::Gte => ">=",
                    OpConst::Equ => "==",
                    OpConst::Neq => "!=",
                };
                if *reg_left {
                    write!(f, "v{} {} {}", register, op_str, ConstPrinter(constant))
                } else {
                    write!(f, "{} {} v{}", ConstPrinter(constant), op_str, register)
                }
            }
            InsType::Cast { var, typed } => write!(f, "cast.{} v{}", typed, var),
            InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => write!(
                f,
                "if v{} then jmp b{} else jmp b{}",
                condvar, iftrue, iffalse
            ),
            InsType::Jmp(n) => write!(f, "jmp b{}", n),
        }
    }
}

pub struct ContextPrinter<'a>(pub Option<&'a Rc<FunctionName>>, pub &'a Context);

impl<'a> std::fmt::Display for ContextPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "-> {} =>", self.1.rettype)?;
        for (idx, typed) in self.1.variables.iter().enumerate() {
            if typed != &Type::NeverUsed {
                writeln!(f, "\tv{}: {}", idx, typed)?;
            }
        }
        if self.1.blocks.is_empty() {
            write!(f, "\tpass")?;
        } else {
            for (idx, (block, block_vars)) in self
                .1
                .blocks
                .iter()
                .zip(self.1.block_vars.iter())
                .enumerate()
            {
                writeln!(f, "b{}:", idx)?;
                writeln!(f, "- preds: {:?}", block.preds)?;
                writeln!(f, "- succs: {:?}", block.succs)?;
                writeln!(
                    f,
                    "- vars_declared_in_this_block: {:?}",
                    block_vars.vars_declared_in_this_block
                )?;
                writeln!(f, "- vars_used: {:?}", block_vars.vars_used)?;
                writeln!(f, "- vars_imported: {:?}", block_vars.vars_imported)?;
                writeln!(
                    f,
                    "- vars_total_imported: {:?}",
                    block_vars.vars_total_imported
                )?;
                writeln!(f, "- vars_exported: {:?}", block_vars.vars_exported)?;
                for ins in &block.ins {
                    writeln!(f, "\t{}", InsPrinter(ins))?;
                }
                if let Some(postlude) = block.postlude.as_ref() {
                    writeln!(f, "\t{}", InsPrinter(postlude))?;
                }
            }
        }
        Ok(())
    }
}

pub struct StructPrinter<'a>(pub &'a StructType);

impl<'a> std::fmt::Display for StructPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "struct {} =>", self.0.name())?;
        for (idx, field) in self.0.fields().iter().enumerate() {
            writeln!(f, "\t{}: {}", idx, field.typed)?;
        }
        Ok(())
    }
}

pub struct ProgramPrinter<'a>(pub &'a Program);

impl<'a> std::fmt::Display for ProgramPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for astruct in self.0.builtins.structs.values() {
            writeln!(f, "{}", StructPrinter(astruct))?;
        }
        for (name, context) in &self.0.contexts {
            writeln!(f, "{}", ContextPrinter(Some(name), context))?;
        }
        Ok(())
    }
}
