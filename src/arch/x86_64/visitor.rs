use crate::arch::context;
use crate::arch::x86_64::{encoder, isa};
use crate::ssa::isa::*;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;

static ARG_REGS: [isa::Reg; 6] = [
    isa::Reg::Rdi,
    isa::Reg::Rsi,
    isa::Reg::Rdx,
    isa::Reg::Rcx,
    isa::Reg::R8,
    isa::Reg::R9,
];

static ALLOC_REGS: [isa::Reg; 14] = [
    isa::Reg::R15,
    isa::Reg::R14,
    isa::Reg::R13,
    isa::Reg::R12,
    isa::Reg::R11,
    isa::Reg::R10,
    isa::Reg::Rbx,
    isa::Reg::R9,
    isa::Reg::R8,
    isa::Reg::Rax,
    isa::Reg::Rcx,
    isa::Reg::Rdx,
    isa::Reg::Rsi,
    isa::Reg::Rdi,
];

static CALLEE_SAVED: [isa::Reg; 5] = [
    isa::Reg::R15,
    isa::Reg::R14,
    isa::Reg::R13,
    isa::Reg::R12,
    isa::Reg::Rbx,
];

static CALLER_SAVED: [isa::Reg; 2] = [isa::Reg::R11, isa::Reg::R10];

static RETURN_REG: isa::Reg = isa::Reg::Rax;

#[derive(Debug, Clone)]
pub enum Error {
    None,
}

pub struct Codegen {
    unflattened_code: HashMap<Rc<FunctionName>, Vec<isa::Block>>,
    constant_operands: BTreeMap<usize, isa::Operand>,
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            unflattened_code: HashMap::new(),
            constant_operands: BTreeMap::new(),
        }
    }

    pub fn process(&mut self, program: &Program) -> Result<context::Contexts, Error> {
        dbg_println!("processing {:#?}", program);
        let mut isa_contexts = context::Contexts::new();
        for (name, context) in &program.contexts {
            self.visit_context(&context, &name, &mut isa_contexts);
        }
        if let Some(main) = self.unflattened_code.get_mut(&FunctionName {
            name: Rc::from("main"),
            arg_types: vec![]
        }) {
            if let Some(block) = main.first_mut() {
                block.ins.insert(0, isa::Ins::InlineBytes(vec![
                    // push rsp
                    0x54,
                    // push qword [rsp]
                    0xff, 0x34, 0x24,
                    // and rsp, -0x10
                    0x48, 0x83, 0xe4, 0xf0,
                ]));
                block.ins.push(isa::Ins::InlineBytes(vec![
                    // mov rsp, qword [rsp + 8]
                    0x48, 0x8b, 0x64, 0x24, 0x08,
                    // ret
                    0xc3,
                ]));
            }
        }
        for (name, context) in &self.unflattened_code {
            isa_contexts.insert(name.clone(), context::Function::Context(encoder::encode_blocks(&context)));
        }
        Ok(isa_contexts)
    }

    pub fn visit_context(&mut self,
        context: &Context, name: &Rc<FunctionName>, isa_contexts: &mut context::Contexts) {
        if self.visit_intrinsic(context, name, isa_contexts) {
            return;
        }
        let mut blocks = vec![];
        self.constant_operands.clear();
        for (idx, block) in context.blocks.iter().enumerate() {
            let mut isa_ins = vec![];
            dbg_println!("{:#?}", block);
            for ins in &block.ins {
                self.visit_ins(ins, &mut isa_ins, idx, context);
            }
            dbg_println!("{:#?}", isa_ins);
            blocks.push(isa::Block { ins: isa_ins });
        }
        dbg_println!("---\nbefore: {:#?}\n===", blocks);
        self.allocate_registers(&mut blocks, context);
        self.setup_stack_and_locals(&mut blocks);
        self.remove_unnecessary_movs(&mut blocks);
        self.remove_unnecessary_jmps(&mut blocks);
        dbg_println!("after: {:#?}\n---", blocks);
        self.unflattened_code.insert(name.clone(), blocks);
    }

    fn get_register_for_var(&mut self, arg: usize) -> isa::Operand {
        self.constant_operands
            .get(&arg)
            .cloned()
            .unwrap_or(isa::Operand::UndeterminedMapping(arg))
    }

    fn type_to_operand_size(typed: &Type) -> Option<isa::OperandSize> {
        match typed {
            Type::Nil => None,
            Type::I32 => Some(isa::OperandSize::I32),
            Type::I64 => Some(isa::OperandSize::I64),
            _ => unimplemented!(),
        }
    }

    pub fn visit_ins(
        &mut self,
        ins: &Ins,
        isa_ins: &mut Vec<isa::Ins>,
        block_idx: usize,
        context: &Context,
    ) {
        dbg_println!("ins: {:#?}", ins);
        match &ins.typed {
            InsType::Nop => (),
            InsType::LoadVar(arg) => {
                isa_ins.push(isa::Ins::Mov(isa::TwoOperands {
                    dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                    src: self.get_register_for_var(*arg),
                    size: Codegen::type_to_operand_size(&context.variables[*arg]).unwrap(),
                }));
            }
            InsType::LoadArg(arg) => {
                isa_ins.push(isa::Ins::Mov(isa::TwoOperands {
                    dest: isa::Operand::UndeterminedMapping(ins.retvar().unwrap()),
                    src: isa::Operand::Register(ARG_REGS[*arg]),
                    size: Codegen::type_to_operand_size(&context.variables[*arg]).unwrap(),
                }));
            }
            InsType::LoadI32(x) => {
                self.constant_operands
                    .insert(ins.retvar().unwrap(), isa::Operand::U32(*x as u32));
            }
            InsType::LoadI64(x) => {
                self.constant_operands
                    .insert(ins.retvar().unwrap(), isa::Operand::U64(*x as u64));
            }
            InsType::Call { name, args } => {
                let mut clobbers: Vec<(isa::Reg, Option<usize>)> =
                    CALLER_SAVED.iter().map(|reg| (*reg, None)).collect();
                for (idx, &reg) in ARG_REGS.iter().enumerate() {
                    if let Some(arg) = args.get(idx).cloned() {
                        clobbers.push((reg, Some(arg)));
                    } else {
                        clobbers.push((reg, None));
                    }
                }
                clobbers.push((RETURN_REG, None));
                // FIXME: check for no return/struct returns
                isa_ins.push(isa::Ins::Clobber(clobbers.clone()));
                for (&arg, &reg) in args.iter().zip(ARG_REGS.iter()) {
                    isa_ins.push(isa::Ins::Mov(isa::TwoOperands {
                        dest: isa::Operand::Register(reg),
                        src: self.get_register_for_var(arg),
                        size: Codegen::type_to_operand_size(&context.variables[arg]).unwrap(),
                    }));
                }
                isa_ins.push(isa::Ins::Call(name.clone()));
                // We store the clobbered values in Unclobber to extend the lifetime of the variables
                // until the end of the call instruction
                isa_ins.push(isa::Ins::Unclobber(clobbers.clone()));
                let retvar = ins.retvar().unwrap();
                if let Some(size) = Codegen::type_to_operand_size(&context.variables[retvar]) {
                    isa_ins.push(isa::Ins::Mov(isa::TwoOperands {
                        dest: isa::Operand::UndeterminedMapping(retvar),
                        src: isa::Operand::Register(RETURN_REG),
                        size,
                    }));
                }
            }
            InsType::Return(x) => {
                isa_ins.push(isa::Ins::Clobber(vec![(isa::Reg::Rax, Some(*x))]));
                isa_ins.push(isa::Ins::Mov(isa::TwoOperands {
                    dest: isa::Operand::Register(isa::Reg::Rax),
                    src: self.get_register_for_var(*x),
                    size: Codegen::type_to_operand_size(&context.variables[*x]).unwrap(),
                }));
                isa_ins.push(isa::Ins::Ret);
            }
            InsType::IfJmp {
                condvar,
                iftrue,
                iffalse,
            } => {
                match isa_ins.last() {
                    Some(isa::Ins::Lt(threeops))
                    | Some(isa::Ins::Gt(threeops))
                    | Some(isa::Ins::Lte(threeops))
                    | Some(isa::Ins::Gte(threeops))
                        if threeops.dest == *condvar =>
                    {
                        let threeops = threeops.clone();
                        let last = isa_ins.pop().unwrap();
                        isa_ins.push(isa::Ins::Cmp {
                            ops: isa::TwoOperands {
                                dest: self.get_register_for_var(threeops.left),
                                src: self.get_register_for_var(threeops.right),
                                size: Codegen::type_to_operand_size(&context.variables[*condvar]).unwrap(),
                            },
                            is_postlude: true,
                        });
                        match last {
                            isa::Ins::Lt(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins::Jge(*iffalse));
                                } else {
                                    isa_ins.push(isa::Ins::Jlt(*iftrue));
                                    isa_ins.push(isa::Ins::Jmp(*iffalse));
                                }
                            }
                            isa::Ins::Gt(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins::Jle(*iffalse));
                                } else {
                                    isa_ins.push(isa::Ins::Jgt(*iftrue));
                                    isa_ins.push(isa::Ins::Jmp(*iffalse));
                                }
                            }
                            isa::Ins::Lte(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins::Jgt(*iffalse));
                                } else {
                                    isa_ins.push(isa::Ins::Jle(*iftrue));
                                    isa_ins.push(isa::Ins::Jmp(*iffalse));
                                }
                            }
                            isa::Ins::Gte(_) => {
                                if *iftrue == block_idx + 1 {
                                    isa_ins.push(isa::Ins::Jlt(*iffalse));
                                } else {
                                    isa_ins.push(isa::Ins::Jge(*iftrue));
                                    isa_ins.push(isa::Ins::Jmp(*iffalse));
                                }
                            }
                            _ => unreachable!(),
                        }
                        return;
                    }
                    _ => (),
                }
                isa_ins.push(isa::Ins::IfJmp {
                    condvar: isa::Operand::UndeterminedMapping(*condvar),
                    iftrue: *iftrue,
                    iffalse: *iffalse,
                });
            }
            InsType::Jmp(x) => {
                isa_ins.push(isa::Ins::Jmp(*x));
            }
            InsType::Lt((x, y))
            | InsType::Gt((x, y))
            | InsType::Lte((x, y))
            | InsType::Gte((x, y)) => {
                let operands = isa::VirtualThreeOperands {
                    dest: ins.retvar().unwrap(),
                    left: *x,
                    right: *y,
                };
                isa_ins.push(match &ins.typed {
                    InsType::Lt(_) => isa::Ins::Lt(operands),
                    InsType::Gt(_) => isa::Ins::Gt(operands),
                    InsType::Lte(_) => isa::Ins::Lte(operands),
                    InsType::Gte(_) => isa::Ins::Gte(operands),
                    _ => unreachable!(),
                });
            }
            InsType::Add((x, y))
            | InsType::Sub((x, y))
            | InsType::Mul((x, y))
            | InsType::Div((x, y)) => {
                let left = &context.variables[*x];
                let right = &context.variables[*y];
                match (left, right) {
                    (Type::I32, Type::I32) |
                    (Type::I64, Type::I64) => {
                        let dest = isa::Operand::UndeterminedMapping(ins.retvar().unwrap());
                        isa_ins.push(isa::Ins::Mov(isa::TwoOperands {
                            dest: dest.clone(),
                            src: self.get_register_for_var(*x),
                            size: Codegen::type_to_operand_size(left).unwrap(),
                        }));
                        isa_ins.push({
                            let operands = isa::TwoOperands {
                                dest: dest.clone(),
                                src: self.get_register_for_var(*y),
                                size: Codegen::type_to_operand_size(left).unwrap(),
                            };
                            match &ins.typed {
                                InsType::Add(_) => isa::Ins::Add(operands),
                                InsType::Sub(_) => isa::Ins::Sub(operands),
                                InsType::Mul(_) => isa::Ins::Mul(operands),
                                InsType::Div(_) => isa::Ins::Div(operands),
                                _ => unreachable!(),
                            }
                        });
                    }
                    (_, _) => unimplemented!(),
                }
            }
            InsType::Exit => (),
            _ => unimplemented!(),
        }
    }

    pub fn visit_intrinsic(&mut self, context: &Context, name: &Rc<FunctionName>, isa_contexts: &mut context::Contexts) -> bool {
        match context.intrinsic {
            IntrinsicType::None => {
                false
            }
            IntrinsicType::Extern(generic) => {
                isa_contexts.insert(name.clone(), context::Function::Extern(generic.clone()));
                true
            }
        }
    }

    pub fn allocate_registers(&mut self, blocks: &mut Vec<isa::Block>, context: &Context) {
        self.allocate_registers_for_block(0, blocks, context, ALLOC_REGS.to_vec(), BTreeMap::new());
    }

    fn allocate_registers_for_block(
        &mut self,
        node: usize,
        blocks: &mut Vec<isa::Block>,
        context: &Context,
        mut unused_regs: Vec<isa::Reg>,
        // Map of variable to (register, allocated interval).
        // if register is none, additional work must be done
        // to retrieve it from local stack.
        mut var_to_reg: BTreeMap<usize, (Option<isa::Reg>, usize)>,
    ) {
        let cblock = &context.blocks[node];
        let mut isa_block = std::mem::replace(&mut blocks[node], isa::Block { ins: vec![] });
        // We defer despilling of the output variables to cleanup stage
        // or when we run out of variables
        let mut variable_factored: BTreeSet<usize> = cblock.vars_out.clone();
        // A map of (ins index, variable set) which maps instruction indices
        // to the variables deallocated at that timestep
        let mut deallocation: BTreeMap<usize, BTreeSet<usize>> = btreemap![];
        // Infer lifetimes for each variable
        for (idx, ins) in isa_block.ins.iter().enumerate().rev() {
            ins.each_used_var(true, |var| match var {
                isa::Operand::UndeterminedMapping(var) => {
                    if !variable_factored.contains(&var) {
                        if let Some(set) = deallocation.get_mut(&idx) {
                            set.insert(*var);
                        } else {
                            deallocation.insert(idx, btreeset![*var]);
                        }
                        variable_factored.insert(*var);
                    }
                }
                _ => (),
            });
        }

        let mut prelude = vec![];
        let mut body = vec![];
        let mut postlude = vec![];
        for (idx, ins) in isa_block.ins.iter_mut().enumerate() {
            dbg_println!(
                "!!! {:#?} {:?} {:?} {:?}",
                ins,
                unused_regs,
                var_to_reg,
                deallocation
            );
            match &ins {
                isa::Ins::Clobber(clobbers) => {
                    dbg_println!("clobbering: {:?}", clobbers);
                    for (reg, _) in clobbers {
                        unused_regs.remove_item(&reg);
                    }
                    let mut set = deallocation.get_mut(&idx);
                    for (reg, except_for_var) in clobbers {
                        let mut to_remove_reg = None;
                        let mut to_remove_var = None;
                        for (var, vdata) in &mut var_to_reg {
                            if vdata.0 == Some(*reg) {
                                if Some(*var) == *except_for_var {
                                    break;
                                }
                                if let Some(set) = set.as_mut() {
                                    if set.remove(&var) {
                                        dbg_println!("dealloc from clobber {:?}", var);
                                        to_remove_var = Some(*var);
                                        break;
                                    }
                                }
                                if let Some(newreg) = unused_regs.pop() {
                                    body.push(isa::Ins::Mov(isa::TwoOperands {
                                        dest: isa::Operand::Register(newreg),
                                        src: isa::Operand::Register(*reg),
                                        size: Codegen::type_to_operand_size(&context.variables[*var]).unwrap(),
                                    }));
                                    vdata.0.replace(newreg);
                                } else {
                                    body.push(isa::Ins::Mov(isa::TwoOperands {
                                        dest: isa::Operand::UndeterminedMapping(*var),
                                        src: isa::Operand::Register(*reg),
                                        size: Codegen::type_to_operand_size(&context.variables[*var]).unwrap(),
                                    }));
                                    to_remove_reg = Some(*var);
                                }
                                break;
                            }
                        }
                        if let Some(to_remove_reg) = to_remove_reg {
                            dbg_println!(
                                " => moving var {} ({:?})",
                                to_remove_reg,
                                var_to_reg[&to_remove_reg].0
                            );
                            let var_ref = var_to_reg.get_mut(&to_remove_reg).unwrap();
                            var_ref.0.take();
                        } else if let Some(to_remove_var) = to_remove_var {
                            var_to_reg.remove(&to_remove_var);
                        }
                    }
                    continue;
                }
                isa::Ins::Unclobber(clobbers) => {
                    for (reg, _) in clobbers {
                        debug_assert!(!unused_regs.contains(reg));
                        unused_regs.push(*reg);
                    }
                    continue;
                }
                _ => (),
            }
            let is_mov = ins.is_mov();
            ins.rename_var_by(false, |var, other_operand| {
                dbg_println!(" => {:?} {:?} {:?}", var, var_to_reg, deallocation);
                if let isa::Operand::UndeterminedMapping(mapping) = var.clone() {
                    if let Some(maybe_reg) = var_to_reg.get_mut(&mapping) {
                        match maybe_reg.clone() {
                            (Some(reg), _) => {
                                *var = isa::Operand::Register(reg);
                            }
                            (None, _) => {
                                // The variable used to store this node has previously
                                // been clobbered, so hopefully we can retrieve
                                // the clobbered register again
                                let reg = unused_regs.pop().unwrap();
                                body.push(isa::Ins::Mov(isa::TwoOperands {
                                    dest: isa::Operand::Register(reg),
                                    src: var.clone(),
                                    size: Codegen::type_to_operand_size(&context.variables[mapping]).unwrap(),
                                }));
                                maybe_reg.0 = Some(reg);
                            }
                        }
                    } else {
                        if unused_regs.is_empty() {
                            // save one of the vars to stack
                            unimplemented!();
                        }
                        let mut reg = None;
                        if is_mov {
                            if let isa::Operand::Register(other_reg) = other_operand {
                                if unused_regs.remove_item(&other_reg).is_some() {
                                    dbg_println!("using other operand: {:?}", other_reg);
                                    reg = Some(*other_reg);
                                }
                            }
                        }
                        let reg = reg.unwrap_or_else(|| unused_regs.pop().unwrap());
                        // Unspill the local variable if it's used in the precceding blocks
                        if cblock.vars_in.contains(&mapping) {
                            // FIXME: correct mov type pls
                            body.push(isa::Ins::Mov(isa::TwoOperands {
                                dest: isa::Operand::Register(reg),
                                src: var.clone(),
                                size: Codegen::type_to_operand_size(&context.variables[mapping]).unwrap(),
                            }));
                        }
                        *var = isa::Operand::Register(reg);
                        var_to_reg.insert(mapping, (Some(reg), idx));
                    }
                    if let Some(set) = deallocation.get_mut(&idx) {
                        if set.remove(&mapping) {
                            let (reg, _) = var_to_reg.remove(&mapping).unwrap();
                            unused_regs.push(reg.unwrap());
                        }
                    }
                }
                dbg_println!("  => {:?}", var);
                dbg_println!("  => {:?}", unused_regs);
            });
            if ins.is_postlude() {
                postlude.push(ins.clone());
            } else {
                body.push(ins.clone());
            }
        }

        // cleanup and create new envs
        let mut new_var_to_reg: BTreeMap<usize, (Option<isa::Reg>, usize)> = BTreeMap::new();
        for (var, (reg, _)) in &var_to_reg {
            if let Some(reg) = reg.clone() {
                if cblock.vars_out.contains(&var) {
                    if cblock.vars_in.contains(&var) {
                        body.push(isa::Ins::Mov(isa::TwoOperands {
                            dest: isa::Operand::UndeterminedMapping(*var),
                            src: isa::Operand::Register(reg),
                            size: Codegen::type_to_operand_size(&context.variables[*var]).unwrap(),
                        }));
                        unused_regs.push(reg);
                    } else {
                        new_var_to_reg.insert(*var, (Some(reg), 0));
                    }
                }
            }
        }

        // insert to new block
        let new_isa_block = &mut blocks[node];
        new_isa_block.ins.append(&mut prelude);
        new_isa_block.ins.append(&mut body);
        new_isa_block.ins.append(&mut postlude);

        dbg_println!("free vars {:?}", unused_regs);
        dbg_println!("new_var_to_reg {:?}", new_var_to_reg);
        dbg_println!(
            "dealloc: {:#?}\n{:#?}\n{:#?}\n{:#?}",
            deallocation,
            isa_block,
            cblock,
            new_isa_block
        );

        // Repeat the process for each descendent
        for succ in &cblock.succs {
            // Since this is a DFS tree, all non-looping descendents
            // must have larger indices that its parent
            if *succ > node {
                self.allocate_registers_for_block(
                    *succ,
                    blocks,
                    context,
                    unused_regs.clone(),
                    new_var_to_reg.clone(),
                );
            }
        }
    }

    pub fn setup_stack_and_locals(&mut self, blocks: &mut Vec<isa::Block>) {
        dbg_println!("stacks for: {:#?}", blocks);
        let mut map_to_stack_offset = BTreeMap::new();
        let stack_offset = 8; // skip 1 dword value for saved rbp
        let mut alloc = 0u32;
        let mut stack_used = false;
        let mut save_regs = BTreeSet::new();
        for block in blocks.iter_mut() {
            for ins in &mut block.ins {
                if let Some(size) = ins.mov_size() {
                    ins.rename_var_by(true, |var, _| match var.clone() {
                        isa::Operand::UndeterminedMapping(mapping) => {
                            let disp = if let Some(offset) = map_to_stack_offset.get(&mapping) {
                                *offset
                            } else {
                                let offset = alloc;
                                map_to_stack_offset.insert(mapping, offset);
                                alloc += size as u32;
                                offset
                            };
                            *var = isa::Operand::Memory {
                                disp: -(disp as i32) - stack_offset,
                                base: isa::Reg::Rbp,
                            };
                        }
                        isa::Operand::Memory {
                            base: isa::Reg::Rsp,
                            ..
                        }
                        | isa::Operand::Memory {
                            base: isa::Reg::Rbp,
                            ..
                        } => {
                            stack_used = true;
                        }
                        isa::Operand::Register(reg) => {
                            if CALLEE_SAVED.contains(&reg) {
                                save_regs.insert(reg);
                                stack_used = true;
                            }
                        }
                        _ => (),
                    });
                }
            }
        }
        if alloc != 0 || stack_used {
            let save_regs: Vec<isa::Reg> = save_regs.iter().cloned().collect();
            if let Some(block) = blocks.first_mut() {
                block.ins.insert(
                    0,
                    isa::Ins::Enter {
                        local_size: alloc + (stack_offset as u32),
                        save_regs: save_regs.clone(),
                    },
                );
            }
            for block in blocks.iter_mut() {
                if let Some(last) = block.ins.last_mut() {
                    if last.is_ret() {
                        *last = isa::Ins::LeaveAndRet {
                            save_regs: save_regs.clone(),
                        };
                    }
                }
            }
        }
    }

    pub fn remove_unnecessary_movs(&mut self, blocks: &mut Vec<isa::Block>) {
        for block in blocks {
            block.ins.retain(|ins| match &ins {
                isa::Ins::Mov(operands) => {
                    if operands.dest == operands.src {
                        false
                    } else {
                        true
                    }
                }
                _ => true,
            })
        }
    }

    pub fn remove_unnecessary_jmps(&mut self, blocks: &mut Vec<isa::Block>) {
        for (idx, block) in blocks.iter_mut().enumerate() {
            if let Some(target) = block
                .ins
                .last()
                .map(|last| match &last {
                    isa::Ins::Jmp(x) => Some(*x),
                    _ => None,
                })
                .flatten()
            {
                if target == idx + 1 {
                    block.ins.pop();
                }
            }
        }
    }
}
