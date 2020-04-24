use crate::ast;
use crate::ast::*;
use crate::compiler;
use crate::ssa::env;
use crate::ssa::env::Env;
use crate::ssa::isa;
use crate::ssa::isa::*;
use crate::utils::optcell::OptCell;
use crate::utils::uniquerc::UniqueRc;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TopLevelInfo {
    pub defstmts: HashMap<Rc<str>, Vec<NodeBox>>,
    pub func_contexts: HashMap<Rc<FunctionName>, Option<Context>>,
    pub types: HashMap<Rc<str>, Type>,
    pub builtins: Builtins,
}

impl TopLevelInfo {
    pub fn new() -> Self {
        let mut generic_fat_pointer_struct = StructType::new(Rc::from("(fat pointer)"));
        generic_fat_pointer_struct.append(Rc::from("address"), Type::ISize);
        generic_fat_pointer_struct.append(Rc::from("len"), Type::ISize);
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
            types: hashmap![
                Rc::from("Nil") => Type::Nil,
                Rc::from("I8") => Type::I8,
                Rc::from("I32") => Type::I32,
                Rc::from("I64") => Type::I64,
                Rc::from("ISize") => Type::ISize,
                Rc::from("F64") => Type::F64,
                Rc::from("Substring") => Type::I8.dyn_slice(),
            ],
            builtins: Builtins {
                structs: hashmap![],
                generic_fat_pointer_struct: Rc::new(generic_fat_pointer_struct),
            },
        }
    }
}

pub struct SSAVisitor<'a> {
    context: Context,
    envs: Vec<Env>,
    top_level: &'a OptCell<TopLevelInfo>,
    has_direct_return: bool,
    has_break: bool,
    last_retvar: Option<Variable>,
}

macro_rules! check_cf_state {
    ($self:expr, $x:block) => {{
        let _has_direct_return = $self.has_direct_return;
        let _has_break = $self.has_break;
        $self.has_direct_return = false;
        $self.has_break = false;
        $x(
            std::mem::replace(&mut $self.has_direct_return, _has_direct_return),
            std::mem::replace(&mut $self.has_break, _has_break),
        )
    }};
}

macro_rules! propagate_cf_state {
    ($self:expr) => {{
        if $self.has_direct_return || $self.has_break {
            break;
        }
    }};
}

impl<'a> SSAVisitor<'a> {
    pub fn new(top_level: &'a OptCell<TopLevelInfo>) -> Self {
        Self {
            context: Context::new(Rc::from("main")),
            envs: Vec::with_capacity(4),
            top_level,
            has_direct_return: false,
            has_break: false,
            last_retvar: None,
        }
    }

    pub fn derive_with_context(&mut self, context: Context) -> Self {
        Self {
            context,
            envs: Vec::with_capacity(4),
            top_level: self.top_level,
            has_direct_return: false,
            has_break: false,
            last_retvar: None,
        }
    }

    pub fn into_context(self) -> Context {
        self.context
    }

    pub fn into_program(self) -> isa::Program {
        let top_level = self.top_level.take().into_inner().unwrap();
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
                .map(|(key, value)| {
                    if let Some(value) = value {
                        (key, value)
                    } else {
                        unreachable!("unknown function context")
                    }
                })
                .collect(),
            entry,
            builtins: top_level.builtins,
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
            if let Some(var) = env.vars.get(var) {
                return Some(*var);
            }
        }
        None
    }

    fn get_breakable(&mut self) -> Option<&mut env::Env> {
        for env in self.envs.iter_mut().rev() {
            if env.break_idx.is_some() {
                return Some(env);
            }
        }
        None
    }

    fn intrinsic_return_type(defstmt: &DefStatement) -> Option<Type> {
        let intrinsic: &IntrinsicType = &defstmt.intrinsic.borrow();
        match intrinsic {
            IntrinsicType::None => None,
            IntrinsicType::Extern(_) => defstmt
                .return_type
                .as_ref()
                .unwrap()
                .typed
                .clone()
                .into_inner(),
        }
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

    fn insert_cast(&mut self, left_var: Variable, mut typed: Type) -> Variable {
        if self.context.variables[left_var] == typed {
            return left_var;
        }
        let casted = self.context.insert_var(typed.clone());
        self.with_block_mut(|block| {
            block.ins.push(Ins::new(
                casted,
                InsType::Cast {
                    var: left_var,
                    typed: std::mem::replace(&mut typed, Type::NoReturn),
                },
            ));
        });
        casted
    }

    fn build_member_expr(
        &mut self,
        n: &MemberExpr,
        b: &NodeBox,
    ) -> Result<InsType, compiler::Error> {
        n.left.visit(self)?;
        let left_var = self.last_retvar.take().unwrap();
        let mut indices: Vec<MemberExprIndex> = Vec::with_capacity(n.right.len());
        for right in &n.right {
            let last_typed = indices
                .last()
                .map(|index| &index.typed)
                .unwrap_or_else(|| &self.context.variables[left_var])
                .clone();
            match right {
                MemberExprArm::Identifier(string) => {
                    if let Some(struct_type) = self.get_struct_type(&last_typed) {
                        if let Some(field) = struct_type.vars().get(string) {
                            indices.push(MemberExprIndex {
                                var: MemberExprIndexVar::StructIndex(field.idx),
                                typed: field.typed.clone(),
                            });
                        } else {
                            return Err(
                                Error::UnknownStructField(string.clone()).into_compiler_error(b)
                            );
                        }
                    } else {
                        return Err(Error::InvalidLHS.into_compiler_error(b));
                    }
                }
                MemberExprArm::Index(idx) => {
                    idx.visit(self)?;
                    let mut idx_var = self.last_retvar.take().unwrap();
                    assert!(self.context.variables[idx_var].is_int());
                    idx_var = self.insert_cast(idx_var, Type::ISize);
                    let typed = last_typed.instance_type().unwrap().clone();
                    indices.push(MemberExprIndex {
                        var: MemberExprIndexVar::Variable(idx_var),
                        typed,
                    });
                }
            }
        }
        let last_typed = &indices.last().unwrap().typed;
        let modifier = if self.is_copyable(last_typed) {
            ReferenceModifier::Copy
        } else {
            ReferenceModifier::Move
        };
        Ok(InsType::MemberReference {
            left: left_var,
            indices,
            modifier,
        })
    }

    fn get_struct_type(&self, typed: &Type) -> Option<Rc<StructType>> {
        match typed {
            maybe_ptr if maybe_ptr.is_fat_pointer() => {
                let top_level = self.top_level.borrow();
                Some(top_level.builtins.generic_fat_pointer_struct.clone())
            }
            Type::Struct(struct_type) => Some(struct_type.clone().into()),
            _ => None,
        }
    }

    fn is_copyable(&self, typed: &Type) -> bool {
        match typed {
            _ if typed.is_fat_pointer() => true,
            Type::Struct(_x) => false,
            Type::Union(_x) => false,
            Type::Slice(x) => self.is_copyable(&x.typed),
            _ => true,
        }
    }
}

impl<'a> Visitor for SSAVisitor<'a> {
    fn visit_program(&mut self, n: &ast::Program) -> VisitorResult {
        let mut top_level_stmts = vec![];
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
            } else if let Some(_) = expr.borrow().downcast_ref::<ClassStatement>() {
                top_level_stmts.push(expr);
            } else if let Some(_) = expr.borrow().downcast_ref::<ImportStatement>() {
                top_level_stmts.push(expr);
            } else {
                outerstmts.push(expr);
            }
        }

        for node in &top_level_stmts {
            node.visit(self)?;
        }

        {
            let top_level = self.top_level.borrow();
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

    fn visit_class(&mut self, n: &ClassStatement, b: &NodeBox) -> VisitorResult {
        let mut struct_type = StructType::new(n.id.clone());
        for inner in &n.inners {
            match inner {
                ClassInner::MemberDef { name, typed } => {
                    self.visit_typeid(typed, b)?;
                    let typed: Type = typed.typed.borrow().clone().unwrap();
                    struct_type.append(name.clone(), typed);
                }
            }
        }
        {
            let mut top_level = self.top_level.borrow_mut();
            let struct_rc = UniqueRc::new(struct_type);
            top_level
                .builtins
                .structs
                .insert(n.id.clone(), struct_rc.clone());
            top_level
                .types
                .insert(n.id.clone(), Type::Struct(struct_rc));
        }
        Ok(())
    }

    fn visit_class_init(&mut self, n: &ClassInitExpr, b: &NodeBox) -> VisitorResult {
        let struct_data = {
            let top_level = self.top_level.borrow();
            if let Some(struct_data) = top_level.builtins.structs.get(&n.id) {
                struct_data.clone()
            } else {
                return Err(Error::UnknownType(n.id.clone()).into_compiler_error(b));
            }
        };
        let retvar = self.context.insert_var(Type::Struct(struct_data.clone()));
        self.with_block_mut(|block| {
            block.ins.push(Ins::new(retvar, InsType::LoadStruct));
        });
        for (name, expr) in &n.inits {
            expr.visit(self)?;
            let right = self.last_retvar.take().unwrap();
            if let Some(field) = struct_data.vars().get(name) {
                if self.context.variables[right] != field.typed {
                    return Err(Error::IncompatibleType {
                        got: self.context.variables[right].clone(),
                        expected: field.typed.clone(),
                    }
                    .into_compiler_error(b));
                }
                let copyable = self.is_copyable(&self.context.variables[right]);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(
                        0,
                        InsType::MemberReferenceStore {
                            left: retvar,
                            indices: vec![MemberExprIndex {
                                var: MemberExprIndexVar::StructIndex(field.idx),
                                typed: field.typed.clone(),
                            }],
                            modifier: if copyable {
                                ReferenceModifier::Copy
                            } else {
                                ReferenceModifier::Move
                            },
                            right,
                        },
                    ));
                });
            } else {
                unimplemented!()
            }
        }
        self.last_retvar = Some(retvar);
        Ok(())
    }

    fn visit_defstmt(&mut self, n: &DefStatement, b: &NodeBox) -> VisitorResult {
        {
            let mut env = Env::new();
            for (idx, def_argument) in n.args.iter().enumerate() {
                env.vars.insert(
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

        self.envs.push(Env::new_breakable());
        check_cf_state!(self, {
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
                    propagate_cf_state!(self);
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
        let env = self.envs.pop().unwrap();

        // Insert jump
        let new_block = self.context.new_block();
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
        for InsPosition { block_idx, ins_idx, } in env.break_idx.unwrap() {
            if let InsType::Jmp(x) = &mut self.context.blocks[block_idx].ins[ins_idx].typed {
                *x = new_block;
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
        let (treturn, tbreak) = check_cf_state!(self, {
            n.cond.visit(self)?;
            cond = self.context.blocks.len() - 1;
            if let Some(last_retvar) = self.last_retvar.take() {
                condvar = last_retvar;
                if self.context.variables[last_retvar] != Type::Bool {
                    return Err(Error::IncompatibleType {
                        got: self.context.variables[last_retvar].clone(),
                        expected: Type::Bool,
                    }
                    .into_compiler_error(b));
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
                    propagate_cf_state!(self);
                }
                iftrue_end = Some(self.context.blocks.len() - 1);
                if let Some(last_retvar) = self.last_retvar.take() {
                    let retvar = self
                        .context
                        .insert_var(self.context.variables[last_retvar].clone());
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(retvar, InsType::Move(last_retvar)));
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
        let (freturn, fbreak) = check_cf_state!(self, {
            if !n.elses.is_empty() {
                iffalse_start = Some(self.context.new_block());
                for node in &n.elses {
                    node.visit(self)?;
                    propagate_cf_state!(self);
                }
                iffalse_end = Some(self.context.blocks.len() - 1);
                if let Some(last_retvar) = self.last_retvar.take() {
                    let retvar = self
                        .context
                        .insert_var(self.context.variables[last_retvar].clone());
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(retvar, InsType::Move(last_retvar)));
                    });
                    iffalse_retvar = Some(retvar);
                }
            }
        });
        self.envs.pop();

        if treturn && treturn == freturn {
            self.has_direct_return = true;
            return Ok(());
        }

        if tbreak && tbreak == fbreak {
            self.has_break = true;
            return Ok(());
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
                block.ins.push(Ins::new(retvar, InsType::Move(last_retvar)));
            }
            if let Some(outer_block) = outer_block {
                block.ins.push(Ins::new(0, InsType::Jmp(outer_block)));
            }
        }
        if let Some(iffalse_end) = iffalse_end {
            let block = &mut self.context.blocks[iffalse_end];
            if let Some(retvar) = retvar {
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                block.ins.push(Ins::new(retvar, InsType::Move(last_retvar)));
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
        if let Some(id) = n
            .callee
            .borrow()
            .downcast_ref::<Value>()
            .map(|val| val.as_identifier())
            .flatten()
        {
            let mut args = Vec::with_capacity(n.args.len());
            let mut arg_types = Vec::with_capacity(n.args.len());
            for arg in &n.args {
                arg.visit(self)?;
                let retvar = self.last_retvar.take().unwrap();
                args.push(retvar);
                arg_types.push(self.context.variables[retvar].clone());
            }
            let mut func_name = Rc::new(FunctionName {
                name: id.clone(),
                arg_types: arg_types.clone(),
            });

            let rettype = {
                let top_level = self.top_level.borrow_mut();
                if let Some((key, maybe_context)) =
                    top_level.func_contexts.get_key_value(&func_name)
                {
                    if let Some(context) = maybe_context {
                        func_name = key.clone();
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
                    let mut usable_defstmt = None;
                    if let Some(vec) = top_level.defstmts.get(id) {
                        for boxed in vec {
                            let defstmt = boxed.rc().downcast_rc::<DefStatement>().unwrap();
                            match defstmt.compatibility_with_args(&arg_types) {
                                ArgCompatibility::None => (),
                                ArgCompatibility::WithCast(casts) => {
                                    for (idx, typed) in casts {
                                        let var = args[idx];
                                        let retvar = self.insert_cast(var, typed.clone());

                                        args[idx] = retvar;
                                        arg_types[idx] = typed.clone();
                                        Rc::get_mut(&mut func_name).unwrap().arg_types[idx] =
                                            typed.clone();
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
                    let func_insert = if top_level.func_contexts.contains_key(&func_name) {
                        true
                    } else {
                        top_level.func_contexts.insert(func_name.clone(), None);
                        false
                    };
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
                        if let Some(rettype) = SSAVisitor::intrinsic_return_type(&defstmt) {
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
                            let mut visitor = self.derive_with_context(func_context);
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
                let block = self.context.block_mut();
                block.ins.push(Ins::new(
                    retvar,
                    InsType::Call {
                        name: func_name.clone(),
                        args: args.clone(),
                    },
                ));
                for arg in &args {
                    if !self.is_copyable(&self.context.variables[*arg]) {
                        let block = self.context.block_mut();
                        block.ins.push(Ins::new(0, InsType::MarkMoved(*arg)));
                    }
                }
            }
            self.last_retvar = Some(retvar);
            return Ok(());
        }
        Err(Error::InvalidLHS.into_compiler_error(b))
    }

    fn visit_letexpr(&mut self, n: &LetExpr, b: &NodeBox) -> VisitorResult {
        if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
            if let Value::Identifier(id) = &id {
                if let Some(lit) = n.right.borrow().downcast_ref::<ast::Value>() {
                    if let ast::Value::Uninitialized = lit {
                        if let Some(type_id) = &n.typed {
                            self.visit_typeid(&type_id, b)?;
                            let typed: Type = type_id.typed.borrow().clone().unwrap();
                            let retvar = self.context.insert_var(typed.clone());
                            match typed {
                                Type::Slice(_) => {
                                    self.with_block_mut(|block| {
                                        block
                                            .ins
                                            .push(Ins::new(retvar, InsType::LoadSlice(vec![])));
                                    });
                                }
                                _ => unimplemented!(),
                            }
                            let env = self.envs.last_mut().unwrap();
                            env.vars.insert(
                                id.clone(),
                                env::Variable {
                                    var: retvar,
                                    is_mut: n.is_mut,
                                },
                            );
                            self.last_retvar = Some(retvar);
                            return Ok(());
                        } else {
                            return Err(Error::CannotInfer.into_compiler_error(b));
                        }
                    }
                }
                n.right.visit(self)?;
                let right = self.last_retvar.take().unwrap();
                let var = self
                    .context
                    .insert_var(self.context.variables[right].clone());
                if self.is_copyable(&self.context.variables[right]) {
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(var, InsType::Copy(right)));
                    });
                } else {
                    self.with_block_mut(|block| {
                        block.ins.push(Ins::new(var, InsType::Move(right)));
                    });
                }
                let env = self.envs.last_mut().unwrap();
                env.vars.insert(
                    id.clone(),
                    env::Variable {
                        var,
                        is_mut: n.is_mut,
                    },
                );
                self.last_retvar = Some(right);
                return Ok(());
            }
        }
        Err(Error::InvalidLHS.into_compiler_error(b))
    }

    fn visit_binexpr(&mut self, n: &BinExpr, b: &NodeBox) -> VisitorResult {
        match &n.op {
            BinOp::Asg | BinOp::Adds | BinOp::Subs | BinOp::Muls | BinOp::Divs | BinOp::Mods => {
                let n_left = n.left.borrow();
                if let Some(id) = n_left.downcast_ref::<ast::Value>() {
                    if let Value::Identifier(id) = &id {
                        let var = if let Some(env_var) = self.get_var(&id) {
                            if !env_var.is_mut {
                                return Err(
                                    Error::MutatingImmutable(id.clone()).into_compiler_error(b)
                                );
                            }
                            env_var.var
                        } else {
                            return Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b));
                        };
                        n.right.visit(self)?;
                        let right = self.last_retvar.take().unwrap();
                        if self.context.variables[var] != self.context.variables[right] {
                            return Err(Error::IncompatibleType {
                                got: self.context.variables[right].clone(),
                                expected: self.context.variables[var].clone(),
                            }
                            .into_compiler_error(b));
                        }
                        match &n.op {
                            BinOp::Asg => {
                                let copyable = self.is_copyable(&self.context.variables[var]);
                                self.with_block_mut(|block| {
                                    if copyable {
                                        block.ins.push(Ins::new(var, InsType::Copy(right)));
                                    } else {
                                        block.ins.push(Ins::new(var, InsType::Move(right)));
                                    }
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
                        Ok(())
                    } else {
                        Err(Error::InvalidLHS.into_compiler_error(b))
                    }
                } else if let Some(member_expr) = n_left.downcast_ref::<ast::MemberExpr>() {
                    if let InsType::MemberReference {
                        left,
                        mut indices,
                        modifier,
                    } = self.build_member_expr(member_expr, &n.left)?
                    {
                        n.right.visit(self)?;
                        let right = self.last_retvar.take().unwrap();
                        match &n.op {
                            BinOp::Asg => {
                                self.with_block_mut(|block| {
                                    block.ins.push(Ins::new(
                                        0,
                                        InsType::MemberReferenceStore {
                                            left,
                                            indices: std::mem::replace(&mut indices, vec![]),
                                            modifier,
                                            right,
                                        },
                                    ));
                                });
                            }
                            _ => unimplemented!(),
                        }
                        self.last_retvar = Some(right);
                        Ok(())
                    } else {
                        unreachable!()
                    }
                } else {
                    Err(Error::InvalidLHS.into_compiler_error(b))
                }
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
                    block.ins.push(Ins::new(result, InsType::Move(left)));
                }

                let (right_block_start, right_block_end);
                {
                    right_block_start = self.context.new_block();
                    n.right.visit(self)?;
                    let right = self.last_retvar.take().unwrap();

                    right_block_end = self.context.new_block();
                    let block = &mut self.context.blocks[right_block_end];
                    block.ins.push(Ins::new(result, InsType::Move(right)));
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
                if self.context.variables[left] != self.context.variables[right] {
                    let left_typed = self.context.variables[left].clone();
                    if self.context.variables[right].can_implicit_cast_to(&left_typed) {
                        right = self.insert_cast(right, left_typed);
                    } else {
                        return Err(Error::IncompatibleType {
                            got: self.context.variables[right].clone(),
                            expected: self.context.variables[left].clone(),
                        }
                        .into_compiler_error(b));
                    }
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
        self.last_retvar = Some(self.insert_cast(retvar, rettype));
        Ok(())
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, b: &NodeBox) -> VisitorResult {
        if let InsType::MemberReference {
            left,
            mut indices,
            modifier,
        } = self.build_member_expr(n, b)?
        {
            let typed = indices.last().unwrap().typed.clone();
            let retvar = self.context.insert_var(typed);
            self.with_block_mut(move |block| {
                block.ins.push(Ins::new(
                    retvar,
                    InsType::MemberReference {
                        left,
                        indices: std::mem::replace(&mut indices, vec![]),
                        modifier,
                    },
                ));
            });
            self.last_retvar = Some(retvar);
            Ok(())
        } else {
            unreachable!()
        }
    }

    fn visit_value(&mut self, n: &Value, b: &NodeBox) -> VisitorResult {
        match n {
            Value::Uninitialized => Err(Error::InternalError.into_compiler_error(b)),
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
            Value::ISize(x) => {
                let retvar = self.context.insert_var(Type::ISize);
                self.with_block_mut(|block| block.ins.push(Ins::new(retvar, InsType::LoadI64(*x))));
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
                let retvar = self
                    .context
                    .insert_var(Type::I8.dyn_slice().pointer(PointerTag::Immutable));
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
                    let copyable = self.is_copyable(&typed);
                    let retvar = self.context.insert_var(typed.slice(slice.len() as u32));
                    self.with_block_mut(|block| {
                        let retvars = std::mem::replace(&mut retvars, vec![]);
                        block
                            .ins
                            .push(Ins::new(retvar, InsType::LoadSlice(retvars.clone())));
                        if !copyable {
                            for retvar in &retvars {
                                block.ins.push(Ins::new(0, InsType::MarkMoved(*retvar)));
                            }
                        }
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
                n.typed.replace(Some(
                    internal
                        .typed
                        .borrow()
                        .clone()
                        .unwrap()
                        .pointer(*pointer_tag),
                ));
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

    fn visit_break(&mut self, _n: &BreakExpr, _b: &NodeBox) -> VisitorResult {
        self.has_break = true;
        let block_idx = self.context.blocks.len() - 1;
        let ins_idx = self.with_block_mut(|block| {
            let idx = block.ins.len();
            block.ins.push(Ins::new(0, InsType::Jmp(0)));
            idx
        });
        let env = self.get_breakable().unwrap();
        env.break_idx.as_mut().unwrap().push(InsPosition {
            block_idx,
            ins_idx,
        });
        Ok(())
    }
}
