use crate::ast;
use crate::ast::*;
use crate::ssa::env::Env;
use crate::ssa::isa;
use crate::ssa::isa::*;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TopLevelInfo {
    pub defstmts: HashMap<Rc<str>, Vec<Rc<DefStatement>>>,
    pub func_contexts: HashMap<Rc<FunctionName>, Option<Context>>,
    pub types: HashMap<Rc<str>, Type>,
}

impl TopLevelInfo {
    pub fn new() -> Self {
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
            types: hashmap![
                Rc::from("Nil") => Type::Nil,
                Rc::from("I32") => Type::I32,
                Rc::from("I64") => Type::I64,
                Rc::from("Float") => Type::Float,
                Rc::from("String") => Type::String,
            ],
        }
    }

    pub fn empty() -> Self {
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct SSAVisitor<'a> {
    context: Context,
    envs: Vec<Env>,
    top_level: &'a RefCell<TopLevelInfo>,
    has_direct_return: bool,
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
            top_level: top_level,
            has_direct_return: false,
        }
    }

    pub fn with_context(context: Context, top_level: &'a RefCell<TopLevelInfo>) -> Self {
        Self {
            context,
            envs: vec![],
            top_level: top_level,
            has_direct_return: false,
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
        }
    }

    fn with_block<T, U>(&self, mut callback: T) -> U
    where
        T: FnMut(&Block) -> U,
    {
        let block = self.context.block();
        callback(block)
    }

    fn with_block_mut<T, U>(&mut self, mut callback: T) -> U
    where
        T: FnMut(&mut Block) -> U,
    {
        let block = self.context.block_mut();
        callback(block)
    }

    fn non_local(&self, var: &Rc<str>) -> Option<usize> {
        for env in self.envs.iter().rev() {
            if let Some(var) = env.vars().get(var) {
                return Some(*var);
            }
        }
        None
    }

    fn last_retvar(&self) -> Option<usize> {
        self.with_block(|block| {
            if let Some(ins) = block.ins.last() {
                ins.retvar()
            } else {
                None
            }
        })
    }

    fn intrinsic_return_type(_intrinsic: &IntrinsicType, _arg_types: &Vec<Type>) -> Option<Type> {
        Some(Type::Nil)
    }

    fn top_level_visit_defstmt(&mut self, defstmt: &DefStatement) -> VisitorResult {
        for (_, typed) in &defstmt.args {
            if let Some(typed) = typed {
                self.visit_typeid(&typed)?;
            }
        }
        if let Some(typed) = defstmt.return_type.as_ref() {
            self.visit_typeid(&typed)?;
        }
        if let Some(attrs) = &defstmt.attrs {
            for attr in attrs {
                match attr.name.as_ref() {
                    "Static" => {
                        defstmt
                            .intrinsic
                            .replace(IntrinsicType::Extern(attr.args[0].to_string()));
                    }
                    _ => return Err(Error::UnknownAttribute(attr.name.clone())),
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
                let defstmt_rc = expr.rc().downcast_rc::<DefStatement>().unwrap();
                if let Some(vec) = top_level.defstmts.get_mut(&defstmt.id) {
                    vec.push(defstmt_rc);
                } else {
                    top_level
                        .defstmts
                        .insert(defstmt.id.clone(), vec![defstmt_rc]);
                }
            } else {
                outerstmts.push(expr);
            }
        }

        {
            let top_level: &TopLevelInfo = &self.top_level.borrow();
            for (_, defstmt_vec) in &top_level.defstmts {
                for defstmt_rc in defstmt_vec {
                    let defstmt: &DefStatement = defstmt_rc.borrow();
                    self.top_level_visit_defstmt(&defstmt)?;
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

    fn visit_defstmt(&mut self, n: &DefStatement) -> VisitorResult {
        {
            let mut env = Env::new();
            for (idx, (name, _)) in n.args.iter().enumerate() {
                env.vars_mut().insert(name.clone(), idx);
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
        if !self.has_direct_return {
            if let Some(retvar) = self.last_retvar() {
                self.context.rettype = self.context.rettype.unify(&self.context.variables[retvar]);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::Return(retvar)));
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
        if let Some(declared_typed) = n.return_type.as_ref() {
            let maybe_declared: &Option<Type> = &declared_typed.typed.borrow();
            if maybe_declared.clone().unwrap() != self.context.rettype {
                return Err(Error::InvalidReturnType);
            }
        }
        Ok(())
    }

    fn visit_return(&mut self, n: &ReturnExpr) -> VisitorResult {
        n.expr.visit(self)?;
        let retvar = self.last_retvar().unwrap();
        let rettype = self.context.variables[retvar].clone();
        if self.context.rettype == Type::NoReturn {
            self.context.rettype = rettype;
        } else if rettype != self.context.rettype {
            self.context.rettype = self.context.rettype.unify(&rettype);
        }
        self.with_block_mut(|block| {
            block.ins.push(Ins::new(retvar, InsType::Return(retvar)));
        });
        self.has_direct_return = true;
        Ok(())
    }

    fn visit_whileexpr(&mut self, n: &WhileExpr) -> VisitorResult {
        let cond_block;
        let while_block;
        let mut while_retvar = None;

        self.envs.push(Env::new());
        let direct_return = check_direct_return!(self, {
            cond_block = Some(self.context.new_block());
            n.cond.visit(self)?;
            if let Some(last_retvar) = self.last_retvar() {
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
                unimplemented!()
            }
            if n.exprs.is_empty() {
                unimplemented!();
            }
            while_block = Some(self.context.new_block());
            if !n.exprs.is_empty() {
                for node in &n.exprs {
                    node.visit(self)?;
                }
                if let Some(last_retvar) = self.last_retvar() {
                    while_retvar = Some(last_retvar);
                }
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(0, InsType::Jmp(cond_block.unwrap())));
                });
            }
        });
        self.envs.pop();

        if direct_return {
            self.has_direct_return = true;
        }

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
        let while_retvar = while_retvar.unwrap();
        self.with_block_mut(|block| {
            block.ins.push(Ins::new(while_retvar, InsType::Nop));
        });

        Ok(())
    }

    fn visit_ifexpr(&mut self, n: &IfExpr) -> VisitorResult {
        let cond = self.context.blocks.len() - 1;
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
            condvar = self.last_retvar().unwrap();
            if let Some(last_retvar) = self.last_retvar() {
                if self.context.variables[last_retvar] != Type::Bool {
                    return Err(Error::IncompatibleType);
                }
            }
            if !n.exprs.is_empty() {
                iftrue_start = Some(self.context.new_block());
                for node in &n.exprs {
                    node.visit(self)?;
                }
                iftrue_end = Some(self.context.blocks.len() - 1);
                if let Some(last_retvar) = self.last_retvar() {
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
                block.ins.push(Ins::new(retvar, InsType::Nop));
            });
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
                if let Some(last_retvar) = self.last_retvar() {
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

        let outer_block = self.context.new_block();

        // Insert jumps
        {
            let block = &mut self.context.blocks[cond];
            block.ins.push(Ins::new(
                0,
                InsType::IfJmp {
                    condvar: condvar,
                    iftrue: iftrue_start.unwrap_or(outer_block),
                    iffalse: iffalse_start.unwrap_or(outer_block),
                },
            ));
        }
        if let Some(iftrue_end) = iftrue_end {
            let block = &mut self.context.blocks[iftrue_end];
            if let Some(retvar) = retvar {
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                block
                    .ins
                    .push(Ins::new(retvar, InsType::LoadVar(last_retvar)));
            }
            block.ins.push(Ins::new(0, InsType::Jmp(outer_block)));
        }
        if let Some(iffalse_end) = iffalse_end {
            let block = &mut self.context.blocks[iffalse_end];
            if let Some(retvar) = retvar {
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                block
                    .ins
                    .push(Ins::new(retvar, InsType::LoadVar(last_retvar)));
            }
            block.ins.push(Ins::new(0, InsType::Jmp(outer_block)));
        }

        Ok(())
    }

    fn visit_callexpr(&mut self, n: &CallExpr) -> VisitorResult {
        if let Some(id) = n.callee.borrow().downcast_ref::<Value>() {
            if let Value::Identifier(id) = &id {
                let mut args = vec![];
                let mut arg_types = vec![];
                for arg in &n.args {
                    arg.visit(self)?;
                    let retvar = self.last_retvar().unwrap();
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
                        return Err(Error::UnknownIdentifier(id.clone()));
                    }
                };

                let retvar = self.context.insert_var(if let Some(rettype) = rettype {
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
                            for defstmt in vec {
                                if defstmt.is_compatible_with_args(&arg_types) {
                                    usable_defstmt = Some(defstmt.clone());
                                    break;
                                }
                            }
                        }
                        (usable_defstmt, func_insert)
                    };
                    if defstmt.is_none() {
                        return Err(Error::InvalidArguments);
                    }
                    let defstmt = defstmt.unwrap();
                    if func_insert {
                        // The function by this name has already been declared
                        if let Some(declared_typed) = defstmt.return_type.as_ref() {
                            declared_typed.typed.borrow().clone().unwrap()
                        } else {
                            return Err(Error::CannotInfer);
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
                                return Err(Error::InternalError);
                            }
                        } else {
                            let (func_name, context) = {
                                let func_context = Context::with_args(id.clone(), arg_types);
                                let mut visitor =
                                    SSAVisitor::with_context(func_context, self.top_level);
                                visitor.visit_defstmt(defstmt.borrow())?;
                                (func_name.clone(), visitor.into_context())
                            };
                            let mut top_level = self.top_level.borrow_mut();
                            let rettype = context.rettype.clone();
                            top_level.func_contexts.insert(func_name, Some(context));
                            rettype
                        }
                    }
                });
                {
                    let args = RefCell::new(Some(args));
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(
                            retvar,
                            InsType::Call {
                                name: func_name.clone(),
                                args: args.replace(None).unwrap(),
                            },
                        ));
                    });
                }
                return Ok(());
            }
        }
        Err(Error::InvalidLHS)
    }

    fn visit_letexpr(&mut self, n: &LetExpr) -> VisitorResult {
        n.right.visit(self)?;
        let right = self.last_retvar().unwrap();
        if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
            if let Value::Identifier(id) = &id {
                let env = self.envs.last_mut().unwrap();
                env.vars_mut().insert(id.clone(), right);
                return Ok(());
            }
        }
        Err(Error::InvalidLHS)
    }

    fn visit_binexpr(&mut self, n: &BinExpr) -> VisitorResult {
        match &n.op {
            BinOp::Asg | BinOp::Adds | BinOp::Subs | BinOp::Muls | BinOp::Divs => {
                if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
                    if let Value::Identifier(id) = &id {
                        if let Some(var) = self.non_local(&id) {
                            n.right.visit(self)?;
                            let right = self.last_retvar().unwrap();
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
                                _ => unreachable!(),
                            }
                            return Ok(());
                        } else {
                            return Err(Error::UnknownIdentifier(id.clone()));
                        }
                    }
                }
                Err(Error::InvalidLHS)
            }
            op => {
                n.left.visit(self)?;
                let left = self.last_retvar().unwrap();
                n.right.visit(self)?;
                let mut right = self.last_retvar().unwrap();
                if self.context.variables[right].can_cast_to(&self.context.variables[left]) {
                    let typed = RefCell::new(Some(self.context.variables[left].clone()));
                    let retvar = self
                        .context
                        .insert_var(self.context.variables[left].clone());
                    self.with_block_mut(move |block| {
                        block.ins.push(Ins::new(
                            retvar,
                            InsType::Cast {
                                var: right,
                                typed: typed.replace(None).unwrap(),
                            },
                        ));
                    });
                    right = retvar;
                } else if self.context.variables[left] != self.context.variables[right] {
                    return Err(Error::IncompatibleType);
                }
                let retvar = self.context.insert_var(match op {
                    BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => Type::Bool,
                    _ => self.context.variables[left].clone(),
                });
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, {
                        match op {
                            BinOp::Add => InsType::Add((left, right)),
                            BinOp::Sub => InsType::Sub((left, right)),
                            BinOp::Mul => InsType::Mul((left, right)),
                            BinOp::Div => InsType::Div((left, right)),
                            BinOp::Lt => InsType::Lt((left, right)),
                            BinOp::Gt => InsType::Gt((left, right)),
                            BinOp::Lte => InsType::Lte((left, right)),
                            BinOp::Gte => InsType::Gte((left, right)),
                            _ => unimplemented!(),
                        }
                    }));
                });
                Ok(())
            }
        }
    }

    fn visit_value(&mut self, n: &Value) -> VisitorResult {
        match n {
            Value::I32(x) => {
                let retvar = self.context.insert_var(Type::I32);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadI32(*x as i32)));
                });
                Ok(())
            }
            Value::I64(x) => {
                let retvar = self.context.insert_var(Type::I64);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadI64(*x)));
                });
                Ok(())
            }
            Value::Float(x) => {
                let retvar = self.context.insert_var(Type::F64);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadF64(*x)));
                });
                Ok(())
            }
            Value::String(x) => {
                let retvar = self.context.insert_var(Type::String);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadString(x.clone())));
                });
                Ok(())
            }
            Value::Identifier(id) => {
                if let Some(retvar) = self.non_local(&id) {
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(retvar, InsType::Nop));
                    });
                    Ok(())
                } else {
                    Err(Error::UnknownIdentifier(id.clone()))
                }
            }
            _ => unimplemented!(),
        }
    }

    fn visit_typeid(&mut self, n: &TypeId) -> VisitorResult {
        match &n.data {
            TypeIdData::Identifier(id) => {
                let top_level = self.top_level.borrow();
                if let Some(typed) = top_level.types.get(id) {
                    n.typed.replace(Some(typed.clone()));
                    Ok(())
                } else {
                    Err(Error::UnknownType(id.clone()))
                }
            }
        }
    }
}
