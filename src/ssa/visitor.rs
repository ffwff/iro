use crate::ast;
use crate::ast::*;
use crate::codegen::structs::*;
use crate::ssa::env;
use crate::ssa::env::Env;
use crate::ssa::isa;
use crate::ssa::isa::*;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TopLevelArch {
    pub pointer_bits: usize,
}

impl TopLevelArch {
    pub fn empty() -> Self {
        TopLevelArch { pointer_bits: 32 }
    }
}

#[derive(Debug)]
pub struct TopLevelInfo {
    pub defstmts: HashMap<Rc<str>, Vec<NodeBox>>,
    pub func_contexts: HashMap<Rc<FunctionName>, Option<Context>>,
    pub types: HashMap<Rc<str>, Type>,
    pub pointer_type: Type,
    pub generic_fat_pointer_struct: Option<StructData>,
}

impl TopLevelInfo {
    pub fn new(arch: TopLevelArch) -> Self {
        let pointer_type = Type::int_from_bits(arch.pointer_bits).unwrap();
        let mut generic_fat_pointer_struct = StructData::new(Rc::from("(fat pointer)"));
        generic_fat_pointer_struct.append_type(Rc::from("address"), pointer_type.clone());
        generic_fat_pointer_struct.append_type(Rc::from("len"), pointer_type.clone());
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
            pointer_type: pointer_type.clone(),
            types: hashmap![
                Rc::from("Nil") => Type::Nil,
                Rc::from("I8") => Type::I8,
                Rc::from("I32") => Type::I32,
                Rc::from("I64") => Type::I64,
                Rc::from("ISize") => pointer_type,
                Rc::from("F64") => Type::F64,
                Rc::from("Substring") => Type::I8.dyn_slice(),
            ],
            generic_fat_pointer_struct: Some(generic_fat_pointer_struct),
        }
    }

    pub fn empty() -> Self {
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
            pointer_type: Type::Nil,
            types: HashMap::new(),
            generic_fat_pointer_struct: None,
        }
    }
}

#[derive(Debug)]
pub struct SSAVisitor<'a> {
    context: Context,
    envs: Vec<Env>,
    top_level: &'a RefCell<TopLevelInfo>,
    has_direct_return: bool,
    last_retvar: Option<Variable>,
}

macro_rules! check_direct_return {
    ($self:expr, $x:block) => {{
        let _has_direct_return = $self.has_direct_return;
        $self.has_direct_return = false;
        $x
        let _retval = $self.has_direct_return;
        $self.has_direct_return = _has_direct_return;
        _retval
    }};
}

impl<'a> SSAVisitor<'a> {
    pub fn new(top_level: &'a RefCell<TopLevelInfo>) -> Self {
        Self {
            context: Context::new(Rc::from("main")),
            envs: vec![],
            top_level,
            has_direct_return: false,
            last_retvar: None,
        }
    }

    pub fn with_context(context: Context, top_level: &'a RefCell<TopLevelInfo>) -> Self {
        Self {
            context,
            envs: vec![],
            top_level,
            has_direct_return: false,
            last_retvar: None,
        }
    }

    pub fn into_context(self) -> Context {
        self.context
    }

    pub fn into_program(self) -> isa::Program {
        let top_level = self.top_level.replace(TopLevelInfo::empty());
        let context = self.context;
        let mut func_contexts = top_level.func_contexts;
        let entry = Rc::new(FunctionName {
            name: context.name.clone(),
            arg_types: vec![],
        });
        func_contexts.insert(entry.clone(), Some(context));
        isa::Program {
            contexts: func_contexts
                .into_iter()
                .map(|(key, value)| (key, value.unwrap()))
                .collect(),
            entry,
            generic_fat_pointer_struct: top_level.generic_fat_pointer_struct.unwrap(),
        }
    }

    fn with_block_mut<T, U>(&mut self, mut callback: T) -> U
    where
        T: FnMut(&mut Block) -> U,
    {
        let block = self.context.block_mut();
        callback(block)
    }

    fn get_var(&self, var: &Rc<str>) -> Option<env::Variable> {
        for env in self.envs.iter().rev() {
            if let Some(var) = env.vars().get(var) {
                return Some(*var);
            }
        }
        None
    }

    fn intrinsic_return_type(_intrinsic: &IntrinsicType, _arg_types: &Vec<Type>) -> Option<Type> {
        Some(Type::Nil)
    }

    fn top_level_visit_defstmt(&mut self, defstmt: &DefStatement, b: &NodeBox) -> VisitorResult {
        for def_argument in &defstmt.args {
            if let Some(typed) = def_argument.type_id.as_ref() {
                self.visit_typeid(&typed, b)?;
            }
        }
        if let Some(typed) = defstmt.return_type.as_ref() {
            self.visit_typeid(&typed, b)?;
        }
        if let Some(attrs) = &defstmt.attrs {
            for attr in attrs {
                match attr.name.as_ref() {
                    "Static" => {
                        defstmt
                            .intrinsic
                            .replace(IntrinsicType::Extern(attr.args[0].to_string()));
                    }
                    _ => {
                        return Err(
                            Error::UnknownAttribute(attr.name.clone()).into_compiler_error(b)
                        )
                    }
                }
            }
        }
        Ok(())
    }
}

impl<'a> Visitor for SSAVisitor<'a> {
    fn visit_program(&mut self, n: &ast::Program) -> VisitorResult {
        let mut outerstmts = vec![];
        for expr in &n.exprs {
            if let Some(defstmt) = expr.borrow().downcast_ref::<DefStatement>() {
                let mut top_level = self.top_level.borrow_mut();
                if let Some(vec) = top_level.defstmts.get_mut(&defstmt.id) {
                    vec.push(expr.clone());
                } else {
                    top_level
                        .defstmts
                        .insert(defstmt.id.clone(), vec![expr.clone()]);
                }
            } else {
                outerstmts.push(expr);
            }
        }

        {
            let top_level: &TopLevelInfo = &self.top_level.borrow();
            for (_, defstmt_vec) in &top_level.defstmts {
                for boxed in defstmt_vec {
                    let defstmt: &DefStatement =
                        boxed.borrow().downcast_ref::<DefStatement>().unwrap();
                    self.top_level_visit_defstmt(&defstmt, &boxed)?;
                }
            }
        }

        self.context.new_block();
        self.envs.push(Env::new());
        for node in &outerstmts {
            node.visit(self)?;
        }
        self.with_block_mut(|block| {
            block.ins.push(Ins::new(0, InsType::Exit));
        });

        Ok(())
    }

    fn visit_import(&mut self, _n: &ImportStatement, _b: &NodeBox) -> VisitorResult {
        unimplemented!()
    }

    fn visit_defstmt(&mut self, n: &DefStatement, b: &NodeBox) -> VisitorResult {
        {
            let mut env = Env::new();
            for (idx, def_argument) in n.args.iter().enumerate() {
                env.vars_mut().insert(
                    def_argument.name.clone(),
                    env::Variable {
                        var: idx,
                        is_mut: def_argument.is_mut,
                    },
                );
            }
            self.envs.push(env);
        }
        if n.exprs.is_empty() {
            self.context.rettype = Type::Nil;
            let retvar = self.context.insert_var(Type::Nil);
            self.with_block_mut(|block| {
                block.ins.extend_from_slice(&[
                    Ins::new(retvar, InsType::LoadNil),
                    Ins::new(0, InsType::Return(retvar)),
                ]);
            });
            return Ok(());
        }
        for expr in &n.exprs {
            expr.visit(self)?;
            if self.has_direct_return {
                break;
            }
        }
        let declared_return: Option<Type> = n
            .return_type
            .as_ref()
            .map(|declared_typed| declared_typed.typed.clone().into_inner())
            .flatten();
        if !self.has_direct_return {
            if let Some(retvar) = self.last_retvar.take() {
                self.context.rettype = self.context.rettype.unify(&self.context.variables[retvar]);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::Return(retvar)));
                });
            } else if declared_return
                .as_ref()
                .map(|typed| *typed == Type::Nil)
                .unwrap_or(false)
            {
                self.context.rettype = Type::Nil;
                let retvar = self.context.insert_var(Type::Nil);
                self.with_block_mut(|block| {
                    block.ins.extend_from_slice(&[
                        Ins::new(retvar, InsType::LoadNil),
                        Ins::new(0, InsType::Return(retvar)),
                    ]);
                });
            } else {
                self.context.rettype = self.context.rettype.unify(&Type::Nil);
                let retvar = self.context.insert_var(Type::Nil);
                self.with_block_mut(|block| {
                    block.ins.extend_from_slice(&[
                        Ins::new(retvar, InsType::LoadNil),
                        Ins::new(0, InsType::Return(retvar)),
                    ]);
                });
            }
        }
        if let Some(declared_return) = declared_return {
            if declared_return != self.context.rettype {
                return Err(Error::InvalidReturnType.into_compiler_error(b));
            }
        }
        Ok(())
    }

    fn visit_return(&mut self, n: &ReturnExpr, _b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        if let Some(retvar) = self.last_retvar.take() {
            let rettype = self.context.variables[retvar].clone();
            if self.context.rettype == Type::NoReturn {
                self.context.rettype = rettype;
            } else if rettype != self.context.rettype {
                self.context.rettype = self.context.rettype.unify(&rettype);
            }
            self.with_block_mut(|block| {
                block.ins.push(Ins::new(retvar, InsType::Return(retvar)));
            });
        }
        self.has_direct_return = true;
        Ok(())
    }

    fn visit_whileexpr(&mut self, n: &WhileExpr, _b: &NodeBox) -> VisitorResult {
        let cond_block;
        let while_block;
        let mut while_retvar = None;

        self.envs.push(Env::new());
        check_direct_return!(self, {
            cond_block = Some(self.context.new_block());
            n.cond.visit(self)?;
            if let Some(last_retvar) = self.last_retvar.take() {
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(
                        0,
                        InsType::IfJmp {
                            condvar: last_retvar,
                            iftrue: cond_block.unwrap(),
                            iffalse: 0usize,
                        },
                    ));
                });
            } else {
                // Condition returns
                return Ok(());
            }
            if n.exprs.is_empty() {
                while_block = Some(self.context.new_block());
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(0, InsType::Jmp(cond_block.unwrap())));
                });

                let new_block = self.context.new_block();
                let retvar = self.context.insert_var(Type::Nil);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadNil));
                });

                {
                    let cond_block = &mut self.context.blocks[cond_block.unwrap()];
                    let ins = cond_block.ins.last_mut().unwrap();
                    if let InsType::IfJmp {
                        iftrue, iffalse, ..
                    } = &mut ins.typed
                    {
                        *iftrue = while_block.unwrap();
                        *iffalse = new_block;
                    } else {
                        unreachable!()
                    }
                }
                self.last_retvar = Some(retvar);
                return Ok(());
            }
            while_block = Some(self.context.new_block());
            if !n.exprs.is_empty() {
                for node in &n.exprs {
                    node.visit(self)?;
                }
                if let Some(last_retvar) = self.last_retvar.take() {
                    while_retvar = Some(last_retvar);
                    self.with_block_mut(|block| {
                        block
                            .ins
                            .push(Ins::new(0, InsType::Jmp(cond_block.unwrap())));
                    });
                }
            }
        });
        self.envs.pop();

        // Insert jump
        {
            let new_block = self.context.new_block();
            let cond_block = &mut self.context.blocks[cond_block.unwrap()];
            let ins = cond_block.ins.last_mut().unwrap();
            if let InsType::IfJmp {
                iftrue, iffalse, ..
            } = &mut ins.typed
            {
                *iftrue = while_block.unwrap();
                *iffalse = new_block;
            } else {
                unreachable!()
            }
        }

        // Insert retvar
        self.last_retvar = while_retvar;
        Ok(())
    }

    fn visit_ifexpr(&mut self, n: &IfExpr, b: &NodeBox) -> VisitorResult {
        let cond;
        let condvar;
        let mut iftrue_start = None;
        let mut iftrue_end = None;
        let mut iffalse_start = None;
        let mut iffalse_end = None;
        let mut iftrue_retvar = None;
        let mut iffalse_retvar = None;

        // If-true branch
        self.envs.push(Env::new());
        let treturn = check_direct_return!(self, {
            n.cond.visit(self)?;
            cond = self.context.blocks.len() - 1;
            if let Some(last_retvar) = self.last_retvar.take() {
                condvar = last_retvar;
                if self.context.variables[last_retvar] != Type::Bool {
                    return Err(Error::IncompatibleType.into_compiler_error(b));
                }
            } else {
                // condition returns
                self.envs.pop();
                return Ok(());
            }
            if !n.exprs.is_empty() {
                iftrue_start = Some(self.context.new_block());
                for node in &n.exprs {
                    node.visit(self)?;
                }
                iftrue_end = Some(self.context.blocks.len() - 1);
                if let Some(last_retvar) = self.last_retvar.take() {
                    let retvar = self
                        .context
                        .insert_var(self.context.variables[last_retvar].clone());
                    self.with_block_mut(|block| {
                        block
                            .ins
                            .push(Ins::new(retvar, InsType::LoadVar(last_retvar)));
                    });
                    iftrue_retvar = Some(retvar);
                }
            }
        });
        self.envs.pop();

        if n.exprs.is_empty() && n.elses.is_empty() {
            // Return nil if two branches don't do anything
            let retvar = self.context.insert_var(Type::Nil);
            self.with_block_mut(|block| {
                block.ins.push(Ins::new(retvar, InsType::LoadNil));
            });
            self.last_retvar = Some(retvar);
            return Ok(());
        }

        // If-false branch
        self.envs.push(Env::new());
        let freturn = check_direct_return!(self, {
            if !n.elses.is_empty() {
                iffalse_start = Some(self.context.new_block());
                for node in &n.elses {
                    node.visit(self)?;
                }
                iffalse_end = Some(self.context.blocks.len() - 1);
                if let Some(last_retvar) = self.last_retvar.take() {
                    let retvar = self
                        .context
                        .insert_var(self.context.variables[last_retvar].clone());
                    self.with_block_mut(|block| {
                        block
                            .ins
                            .push(Ins::new(retvar, InsType::LoadVar(last_retvar)));
                    });
                    iffalse_retvar = Some(retvar);
                }
            }
        });
        self.envs.pop();

        if treturn && treturn == freturn {
            self.has_direct_return = true;
        }

        let retvar_type = {
            match (iftrue_retvar, iffalse_retvar) {
                (Some(first), Some(last)) => {
                    Some(self.context.variables[first].unify(&self.context.variables[last]))
                }
                (Some(first), None) => Some(self.context.variables[first].unify(&Type::Nil)),
                (None, Some(last)) => Some(self.context.variables[last].unify(&Type::Nil)),
                (None, None) => None,
            }
        };
        let retvar = retvar_type.map(|typed| self.context.insert_var(typed));

        let outer_block = if !self.has_direct_return {
            Some(self.context.new_block())
        } else {
            None
        };

        // Insert jumps and return values
        {
            let block = &mut self.context.blocks[cond];
            if let Some(retvar) = retvar {
                if self.context.variables[retvar].is_nil() {
                    block.ins.push(Ins::new(retvar, InsType::LoadNil));
                }
            }
            if !self.has_direct_return {
                block.ins.push(Ins::new(
                    0,
                    InsType::IfJmp {
                        condvar: condvar,
                        iftrue: iftrue_start.unwrap_or(outer_block.unwrap()),
                        iffalse: iffalse_start.unwrap_or(outer_block.unwrap()),
                    },
                ));
            }
        }
        if let Some(iftrue_end) = iftrue_end {
            let block = &mut self.context.blocks[iftrue_end];
            if let Some(retvar) = retvar {
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                block
                    .ins
                    .push(Ins::new(retvar, InsType::LoadVar(last_retvar)));
            }
            if let Some(outer_block) = outer_block {
                block.ins.push(Ins::new(0, InsType::Jmp(outer_block)));
            }
        }
        if let Some(iffalse_end) = iffalse_end {
            let block = &mut self.context.blocks[iffalse_end];
            if let Some(retvar) = retvar {
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                block
                    .ins
                    .push(Ins::new(retvar, InsType::LoadVar(last_retvar)));
            }
            if let Some(outer_block) = outer_block {
                block.ins.push(Ins::new(0, InsType::Jmp(outer_block)));
            }
        }

        // Load return var
        self.last_retvar = retvar;
        Ok(())
    }

    fn visit_callexpr(&mut self, n: &CallExpr, b: &NodeBox) -> VisitorResult {
        if let Some(id) = n.callee.borrow().downcast_ref::<Value>() {
            if let Value::Identifier(id) = &id {
                let mut args = vec![];
                let mut arg_types = vec![];
                for arg in &n.args {
                    arg.visit(self)?;
                    let retvar = self.last_retvar.take().unwrap();
                    args.push(retvar);
                    arg_types.push(self.context.variables[retvar].clone());
                }
                let func_name = Rc::new(FunctionName {
                    name: id.clone(),
                    arg_types: arg_types.clone(),
                });

                let rettype = {
                    let top_level = self.top_level.borrow_mut();
                    if let Some(maybe_context) = top_level.func_contexts.get(&func_name) {
                        if let Some(context) = maybe_context {
                            Some(context.rettype.clone())
                        } else {
                            None
                        }
                    } else if top_level.defstmts.contains_key(id) {
                        None
                    } else {
                        return Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b));
                    }
                };

                let rettype = if let Some(rettype) = rettype {
                    rettype
                } else {
                    let (defstmt, func_insert) = {
                        let mut top_level = self.top_level.borrow_mut();
                        let func_insert = top_level
                            .func_contexts
                            .insert(func_name.clone(), None)
                            .is_some();
                        let mut usable_defstmt = None;
                        if let Some(vec) = top_level.defstmts.get(id) {
                            for boxed in vec {
                                let defstmt = boxed.rc().downcast_rc::<DefStatement>().unwrap();
                                match defstmt.compatibility_with_args(&arg_types) {
                                    ArgCompatibility::None => (),
                                    ArgCompatibility::WithCast(casts) => {
                                        for (idx, typed) in casts {
                                            let retvar = self.context.insert_var(typed.clone());
                                            let var = args[idx];
                                            self.with_block_mut(move |block| {
                                                block.ins.push(Ins::new(
                                                    retvar,
                                                    InsType::Cast {
                                                        var,
                                                        typed: typed.clone(),
                                                    },
                                                ));
                                            });
                                        }
                                        usable_defstmt = Some(defstmt.clone());
                                        break;
                                    }
                                    ArgCompatibility::Full => {
                                        usable_defstmt = Some(defstmt.clone());
                                        break;
                                    }
                                }
                            }
                        }
                        (usable_defstmt, func_insert)
                    };
                    if defstmt.is_none() {
                        return Err(Error::InvalidArguments.into_compiler_error(b));
                    }
                    let defstmt = defstmt.unwrap();
                    if func_insert {
                        // The function by this name has already been declared
                        if let Some(declared_typed) = defstmt.return_type.as_ref() {
                            declared_typed.typed.borrow().clone().unwrap()
                        } else {
                            return Err(Error::CannotInfer.into_compiler_error(b));
                        }
                    } else {
                        // Properly compute the return type
                        if !defstmt.intrinsic.borrow().is_none() {
                            if let Some(rettype) = {
                                SSAVisitor::intrinsic_return_type(
                                    &defstmt.intrinsic.borrow(),
                                    &arg_types,
                                )
                            } {
                                let context = Context::with_intrinsics(
                                    id.clone(),
                                    arg_types,
                                    rettype.clone(),
                                    defstmt.intrinsic.clone().into_inner(),
                                );
                                let mut top_level = self.top_level.borrow_mut();
                                top_level
                                    .func_contexts
                                    .insert(func_name.clone(), Some(context));
                                rettype
                            } else {
                                return Err(Error::InternalError.into_compiler_error(b));
                            }
                        } else {
                            let (func_name, context) = {
                                let func_context = Context::with_args(id.clone(), arg_types);
                                let mut visitor =
                                    SSAVisitor::with_context(func_context, self.top_level);
                                visitor.visit_defstmt(defstmt.borrow(), b)?;
                                (func_name.clone(), visitor.into_context())
                            };
                            let mut top_level = self.top_level.borrow_mut();
                            let rettype = context.rettype.clone();
                            top_level.func_contexts.insert(func_name, Some(context));
                            rettype
                        }
                    }
                };
                let retvar = self.context.insert_var(rettype);
                {
                    let mut args = Some(args);
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(
                            retvar,
                            InsType::Call {
                                name: func_name.clone(),
                                args: args.take().unwrap(),
                            },
                        ));
                    });
                }
                self.last_retvar = Some(retvar);
                return Ok(());
            }
        }
        Err(Error::InvalidLHS.into_compiler_error(b))
    }

    fn visit_letexpr(&mut self, n: &LetExpr, b: &NodeBox) -> VisitorResult {
        n.right.visit(self)?;
        let right = self.last_retvar.take().unwrap();
        if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
            if let Value::Identifier(id) = &id {
                let env = self.envs.last_mut().unwrap();
                env.vars_mut().insert(
                    id.clone(),
                    env::Variable {
                        var: right,
                        is_mut: n.is_mut,
                    },
                );
                return Ok(());
            }
        }
        self.last_retvar = Some(right);
        Err(Error::InvalidLHS.into_compiler_error(b))
    }

    fn visit_binexpr(&mut self, n: &BinExpr, b: &NodeBox) -> VisitorResult {
        match &n.op {
            BinOp::Asg | BinOp::Adds | BinOp::Subs | BinOp::Muls | BinOp::Divs | BinOp::Mods => {
                if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
                    if let Value::Identifier(id) = &id {
                        if let Some(env_var) = self.get_var(&id) {
                            if !env_var.is_mut {
                                return Err(
                                    Error::MutatingImmutable(id.clone()).into_compiler_error(b)
                                );
                            }
                            let var = env_var.var;
                            n.right.visit(self)?;
                            let right = self.last_retvar.take().unwrap();
                            match &n.op {
                                BinOp::Asg => {
                                    self.with_block_mut(|block| {
                                        block.ins.push(Ins::new(var, InsType::LoadVar(right)));
                                    });
                                }
                                BinOp::Adds => {
                                    self.with_block_mut(|block| {
                                        block.ins.push(Ins::new(var, InsType::Add((var, right))));
                                    });
                                }
                                BinOp::Subs => {
                                    self.with_block_mut(|block| {
                                        block.ins.push(Ins::new(var, InsType::Sub((var, right))));
                                    });
                                }
                                BinOp::Muls => {
                                    self.with_block_mut(|block| {
                                        block.ins.push(Ins::new(var, InsType::Mul((var, right))));
                                    });
                                }
                                BinOp::Divs => {
                                    self.with_block_mut(|block| {
                                        block.ins.push(Ins::new(var, InsType::Div((var, right))));
                                    });
                                }
                                BinOp::Mods => {
                                    self.with_block_mut(|block| {
                                        block.ins.push(Ins::new(var, InsType::Mod((var, right))));
                                    });
                                }
                                _ => unreachable!(),
                            }
                            self.last_retvar = Some(var);
                            return Ok(());
                        } else {
                            return Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b));
                        }
                    }
                }
                Err(Error::InvalidLHS.into_compiler_error(b))
            }
            BinOp::And | BinOp::Or => {
                /*
                --- Short circuiting and expression:
                [result = left]
                | jmp(1) if result is false
                | [result = right]
                | jmp(1)
                |-> (1): end block

                --- Short circuiting or expression:
                [result = left]
                | jmp(1) if result is true
                | [result = right]
                | jmp(1)
                |-> (1): end block
                 */
                let result = self.context.insert_var(Type::Bool);

                let left_block_end;
                {
                    self.context.new_block();
                    n.left.visit(self)?;
                    let left = self.last_retvar.take().unwrap();

                    left_block_end = self.context.new_block();
                    let block = &mut self.context.blocks[left_block_end];
                    block.ins.push(Ins::new(result, InsType::LoadVar(left)));
                }

                let (right_block_start, right_block_end);
                {
                    right_block_start = self.context.new_block();
                    n.right.visit(self)?;
                    let right = self.last_retvar.take().unwrap();

                    right_block_end = self.context.new_block();
                    let block = &mut self.context.blocks[right_block_end];
                    block.ins.push(Ins::new(result, InsType::LoadVar(right)));
                }

                // Insert retvar
                let end_block = self.context.new_block();

                // Insert jumps
                {
                    let block = &mut self.context.blocks[left_block_end];
                    match &n.op {
                        BinOp::And => block.ins.push(Ins::new(
                            0,
                            InsType::IfJmp {
                                condvar: result,
                                iftrue: right_block_start,
                                iffalse: end_block,
                            },
                        )),
                        BinOp::Or => block.ins.push(Ins::new(
                            0,
                            InsType::IfJmp {
                                condvar: result,
                                iftrue: end_block,
                                iffalse: right_block_start,
                            },
                        )),
                        _ => unreachable!(),
                    }
                }
                {
                    let block = &mut self.context.blocks[right_block_end];
                    block.ins.push(Ins::new(0, InsType::Jmp(end_block)));
                }

                self.last_retvar = Some(result);
                Ok(())
            }
            op => {
                n.left.visit(self)?;
                let left = self.last_retvar.take().unwrap();
                n.right.visit(self)?;
                let mut right = self.last_retvar.take().unwrap();
                if self.context.variables[right].can_implicit_cast_to(&self.context.variables[left])
                {
                    let mut typed = Some(self.context.variables[left].clone());
                    let retvar = self
                        .context
                        .insert_var(self.context.variables[left].clone());
                    self.with_block_mut(move |block| {
                        block.ins.push(Ins::new(
                            retvar,
                            InsType::Cast {
                                var: right,
                                typed: typed.take().unwrap(),
                            },
                        ));
                    });
                    right = retvar;
                } else if self.context.variables[left] != self.context.variables[right] {
                    return Err(Error::IncompatibleType.into_compiler_error(b));
                }
                let retvar = self.context.insert_var(match op {
                    BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Equ => Type::Bool,
                    _ => self.context.variables[left].clone(),
                });
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, {
                        match op {
                            BinOp::Add => InsType::Add((left, right)),
                            BinOp::Sub => InsType::Sub((left, right)),
                            BinOp::Mul => InsType::Mul((left, right)),
                            BinOp::Div => InsType::Div((left, right)),
                            BinOp::Mod => InsType::Mod((left, right)),
                            BinOp::Lt => InsType::Lt((left, right)),
                            BinOp::Gt => InsType::Gt((left, right)),
                            BinOp::Lte => InsType::Lte((left, right)),
                            BinOp::Gte => InsType::Gte((left, right)),
                            BinOp::Equ => InsType::Equ((left, right)),
                            _ => unimplemented!("{:?}", op),
                        }
                    }));
                });
                self.last_retvar = Some(retvar);
                Ok(())
            }
        }
    }

    fn visit_asexpr(&mut self, n: &AsExpr, b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        let retvar = self.last_retvar.take().unwrap();
        n.typed.visit(self, b)?;
        let rettype = n.typed.typed.borrow().clone().unwrap();
        let new_retvar = self.context.insert_var(rettype.clone());
        let mut rettype = Some(rettype);
        self.with_block_mut(move |block| {
            block.ins.push(Ins::new(
                new_retvar,
                InsType::Cast {
                    var: retvar,
                    typed: rettype.take().unwrap(),
                },
            ));
        });
        self.last_retvar = Some(retvar);
        Ok(())
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, _b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        let leftvar = self.last_retvar.take().unwrap();
        match &n.right {
            MemberExprArm::Identifier(string) => {
                let rettype = match &self.context.variables[leftvar] {
                    Type::Struct(struct_data) => {
                        if let Some(var_data) = struct_data.0.values().get(string) {
                            var_data.typed.clone()
                        } else {
                            unimplemented!()
                        }
                    }
                    typed if typed.is_fat_pointer() => {
                        let top_level = self.top_level.borrow();
                        let struct_data = top_level.generic_fat_pointer_struct.as_ref().unwrap();
                        if let Some(var_data) = struct_data.values().get(string) {
                            var_data.typed.clone()
                        } else {
                            unimplemented!()
                        }
                    }
                    _ => unimplemented!(),
                };
                let retvar = self.context.insert_var(rettype);
                self.last_retvar = Some(retvar);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(
                        retvar,
                        InsType::MemberReference {
                            left: leftvar,
                            right: string.clone(),
                        },
                    ));
                });
            }
            MemberExprArm::Index(idx) => {
                idx.visit(self)?;
                let idx_var = self.last_retvar.take().unwrap();
                assert!(self.context.variables[idx_var].is_int());
                match self.context.variables[leftvar].clone() {
                    Type::I32Ptr(ptr_typed) | Type::I64Ptr(ptr_typed) => {
                        let typed: &Type = &ptr_typed.typed;
                        if let Type::Slice(slice_type) = typed.borrow() {
                            if slice_type.is_dyn() {
                                let retvar = self.context.insert_var(slice_type.typed.clone());
                                let trap_block = self.context.blocks.len();
                                let body_block = self.context.blocks.len() + 1;
                                let condvar = self.context.insert_var(Type::Bool);
                                self.with_block_mut(|block| {
                                    block.ins.push(Ins::new(
                                        condvar,
                                        InsType::BoundsCheck {
                                            var: leftvar,
                                            index: idx_var,
                                        },
                                    ));
                                    block.ins.push(Ins::new(
                                        0,
                                        InsType::IfJmp {
                                            condvar: condvar,
                                            iftrue: body_block,
                                            iffalse: trap_block,
                                        },
                                    ));
                                });
                                // Trap block
                                self.context.new_block();
                                self.with_block_mut(|block| {
                                    block
                                        .ins
                                        .push(Ins::new(0, InsType::Trap(TrapType::BoundsCheck)));
                                });
                                // Body continuation block
                                self.context.new_block();
                                self.with_block_mut(|block| {
                                    block.ins.push(Ins::new(
                                        retvar,
                                        InsType::FatIndex {
                                            var: leftvar,
                                            index: idx_var,
                                        },
                                    ));
                                });
                                self.last_retvar = Some(retvar);
                                return Ok(());
                            }
                        }
                        let retvar = self.context.insert_var((*typed).clone());
                        self.with_block_mut(|block| {
                            block.ins.push(Ins::new(
                                retvar,
                                InsType::PointerIndex {
                                    var: leftvar,
                                    index: idx_var,
                                },
                            ));
                        });
                        self.last_retvar = Some(retvar);
                    }
                    Type::Slice(slice_rc) => {
                        let slice_type: &SliceType = &slice_rc.borrow();
                        let retvar = self.context.insert_var(slice_type.typed.clone());
                        let trap_block = self.context.blocks.len();
                        let body_block = self.context.blocks.len() + 1;
                        let condvar = self.context.insert_var(Type::Bool);
                        self.with_block_mut(|block| {
                            block.ins.push(Ins::new(
                                condvar,
                                InsType::BoundsCheck {
                                    var: leftvar,
                                    index: idx_var,
                                },
                            ));
                            block.ins.push(Ins::new(
                                0,
                                InsType::IfJmp {
                                    condvar: condvar,
                                    iftrue: body_block,
                                    iffalse: trap_block,
                                },
                            ));
                        });
                        // Trap block
                        self.context.new_block();
                        self.with_block_mut(|block| {
                            block
                                .ins
                                .push(Ins::new(0, InsType::Trap(TrapType::BoundsCheck)));
                        });
                        // Body continuation block
                        self.context.new_block();
                        self.with_block_mut(|block| {
                            block.ins.push(Ins::new(
                                retvar,
                                InsType::PointerIndex {
                                    var: leftvar,
                                    index: idx_var,
                                },
                            ));
                        });
                        self.last_retvar = Some(retvar);
                    }
                    other => unimplemented!("{:#?}", other),
                }
            }
        }
        Ok(())
    }

    fn visit_value(&mut self, n: &Value, b: &NodeBox) -> VisitorResult {
        match n {
            Value::I32(x) => {
                let retvar = self.context.insert_var(Type::I32);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadI32(*x as i32)));
                });
                self.last_retvar = Some(retvar);
                Ok(())
            }
            Value::I64(x) => {
                let retvar = self.context.insert_var(Type::I64);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadI64(*x)));
                });
                self.last_retvar = Some(retvar);
                Ok(())
            }
            Value::Float(x) => {
                let retvar = self.context.insert_var(Type::F64);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadF64(*x)));
                });
                self.last_retvar = Some(retvar);
                Ok(())
            }
            Value::Bool(x) => {
                let retvar = self.context.insert_var(Type::Bool);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadBool(*x)));
                });
                self.last_retvar = Some(retvar);
                Ok(())
            }
            Value::String(x) => {
                let retvar = {
                    let top_level = self.top_level.borrow();
                    self.context.insert_var(
                        top_level
                            .pointer_type
                            .ptr_for(Type::I8.dyn_slice(), PointerTag::Immutable)
                            .unwrap(),
                    )
                };
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadSubstring(x.clone())));
                });
                self.last_retvar = Some(retvar);
                Ok(())
            }
            Value::Identifier(id) => {
                if let Some(env_var) = self.get_var(&id) {
                    self.last_retvar = Some(env_var.var);
                    Ok(())
                } else {
                    Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b))
                }
            }
            Value::Slice(slice) => {
                let mut typed = None;
                let mut retvars = vec![];
                for item in slice {
                    item.visit(self)?;
                    let retvar = self.last_retvar.unwrap();
                    if let Some(old_typed) = typed {
                        typed = Some(self.context.variables[retvar].unify(&old_typed));
                    } else {
                        typed = Some(self.context.variables[retvar].clone());
                    }
                    retvars.push(retvar);
                }
                if let Some(typed) = typed {
                    let retvar = self.context.insert_var(typed.slice(slice.len() as u32));
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(
                            retvar,
                            InsType::LoadSlice(std::mem::replace(&mut retvars, vec![])),
                        ));
                    });
                    self.last_retvar = Some(retvar);
                    Ok(())
                } else {
                    Err(Error::CannotInfer.into_compiler_error(b))
                }
            }
        }
    }

    fn visit_typeid(&mut self, n: &TypeId, b: &NodeBox) -> VisitorResult {
        match &n.data {
            TypeIdData::Identifier(id) => {
                let top_level = self.top_level.borrow();
                if n.typed.borrow().is_some() {
                    Ok(())
                } else if let Some(typed) = top_level.types.get(id) {
                    n.typed.replace(Some(typed.clone()));
                    Ok(())
                } else {
                    Err(Error::UnknownType(id.clone()).into_compiler_error(b))
                }
            }
            TypeIdData::Pointer {
                typed: internal,
                pointer_tag,
            } => {
                self.visit_typeid(&internal, b)?;
                let top_level: &TopLevelInfo = &self.top_level.borrow();
                n.typed.replace(
                    top_level
                        .pointer_type
                        .ptr_for(internal.typed.borrow().clone().unwrap(), *pointer_tag),
                );
                Ok(())
            }
            TypeIdData::Slice {
                typed: internal,
                length,
            } => {
                self.visit_typeid(&internal, b)?;
                let typed: &Option<Type> = &internal.typed.borrow();
                if let Some(length) = length {
                    n.typed.replace(Some(typed.clone().unwrap().slice(*length)));
                } else {
                    n.typed.replace(Some(typed.clone().unwrap().dyn_slice()));
                }
                Ok(())
            }
        }
    }
}
