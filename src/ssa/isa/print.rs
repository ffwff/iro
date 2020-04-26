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

struct RegConstPrinter<'a>(pub &'a RegConst, pub &'static str);

impl<'a> RegConstPrinter<'a> {
    pub fn print(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            RegConst::RegLeft((var, k)) => write!(f, "v{} {} {}", var, self.1, ConstPrinter(k)),
            RegConst::RegRight((k, var)) => write!(f, "{} {} v{}", ConstPrinter(k), self.1, var),
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
            InsType::Nop => write!(f, "nop"),
            InsType::LoadNil => write!(f, "nil"),
            InsType::Drop(arg) => write!(f, "drop v{}", arg),
            InsType::Move(var) => write!(f, "move v{}", var),
            InsType::MarkMoved(var) => write!(f, "mark_moved v{}", var),
            InsType::Copy(var) => write!(f, "copy v{}", var),
            InsType::LoadArg(arg) => write!(f, "load_arg {}", arg),
            InsType::LoadI32(n) => write!(f, "load.I32 {}", n),
            InsType::LoadI64(n) => write!(f, "load.I64 {}", n),
            InsType::LoadF64(n) => write!(f, "load.F64 {}", n),
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
                left,
                indices,
                modifier,
                right,
            } => write!(
                f,
                "member v{} [{}] = {} v{}",
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
                    .join(", "),
                match modifier {
                    ReferenceModifier::Copy => "copy",
                    ReferenceModifier::Move => "move",
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
                "call {}({})",
                name.name,
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
            InsType::AddC(rc) => RegConstPrinter(rc, "+").print(f),
            InsType::SubC(rc) => RegConstPrinter(rc, "-").print(f),
            InsType::MulC(rc) => RegConstPrinter(rc, "*").print(f),
            InsType::DivC(rc) => RegConstPrinter(rc, "/").print(f),
            InsType::ModC(rc) => RegConstPrinter(rc, "%").print(f),
            InsType::LtC(rc) => RegConstPrinter(rc, "<").print(f),
            InsType::GtC(rc) => RegConstPrinter(rc, ">").print(f),
            InsType::LteC(rc) => RegConstPrinter(rc, "<=").print(f),
            InsType::GteC(rc) => RegConstPrinter(rc, ">=").print(f),
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
        if let Some(name) = &self.0 {
            write!(
                f,
                "function {} -> {} =>\n",
                name.to_string(),
                self.1.rettype
            )?;
        } else {
            write!(f, "-> {} =>\n", self.1.rettype)?;
        }
        for (idx, typed) in self.1.variables.iter().enumerate() {
            if typed != &Type::NeverUsed {
                write!(f, "\tv{}: {}\n", idx, typed)?;
            }
        }
        if self.1.blocks.is_empty() {
            write!(f, "\tpass\n")?;
        } else {
            for (idx, block) in self.1.blocks.iter().enumerate() {
                write!(f, "b{}:\n", idx)?;
                write!(
                    f,
                    "- vars_declared_in_this_block: {:?}\n",
                    block.vars_declared_in_this_block
                )?;
                write!(f, "- vars_used: {:?}\n", block.vars_used)?;
                write!(f, "- vars_exported: {:?}\n", block.vars_exported)?;
                write!(f, "- vars_block_local: {:?}\n", block.vars_block_local)?;
                for ins in &block.ins {
                    write!(f, "\t{}\n", InsPrinter(ins))?;
                }
                if block.postlude.typed != InsType::Nop {
                    write!(f, "\t{}\n", InsPrinter(&block.postlude))?;
                }
            }
        }
        Ok(())
    }
}

pub struct StructPrinter<'a>(pub &'a StructType);

impl<'a> std::fmt::Display for StructPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {} =>\n", self.0.name())?;
        for (idx, field) in self.0.fields().iter().enumerate() {
            write!(f, "\t{}: {}\n", idx, field.typed)?;
        }
        Ok(())
    }
}

pub struct ProgramPrinter<'a>(pub &'a Program);

impl<'a> std::fmt::Display for ProgramPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, astruct) in &self.0.builtins.structs {
            write!(f, "{}\n", StructPrinter(astruct))?;
        }
        for (name, context) in &self.0.contexts {
            write!(f, "{}\n", ContextPrinter(Some(name), context))?;
        }
        Ok(())
    }
}
