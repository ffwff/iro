use crate::ast;
use crate::ast::*;
use crate::compiler;
use crate::compiler::sources::Sources;
use crate::runtime;
use crate::ssa::env;
use crate::ssa::env::Env;
use crate::ssa::isa;
use crate::ssa::isa::*;
use crate::utils::optcell::OptCell;
use crate::utils::uniquerc::UniqueRc;
use fnv::FnvHashMap;
use smallvec::SmallVec;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::rc::Rc;

pub struct AstTopLevelInfo {
    pub defstmts: FnvHashMap<PathVec, Vec<NodeBox>>,
    pub class_stmts: FnvHashMap<PathVec, NodeBox>,
}

impl AstTopLevelInfo {
    pub fn new() -> Self {
        Self {
            defstmts: FnvHashMap::default(),
            class_stmts: FnvHashMap::default(),
        }
    }
}

pub struct TopLevelInfo {
    pub func_contexts: FnvHashMap<Rc<FunctionName>, Option<Context>>,
    pub types: FnvHashMap<PathVec, Type>,
    pub builtins: Builtins,
}

impl TopLevelInfo {
    pub fn new() -> Self {
        let mut generic_fat_pointer_struct = StructType::new(Rc::from("(fat pointer)"));
        generic_fat_pointer_struct.append(Rc::from("address"), Type::ISize);
        generic_fat_pointer_struct.append(Rc::from("len"), Type::ISize);
        TopLevelInfo {
            func_contexts: fnv_hashmap![],
            types: fnv_hashmap![
                smallvec![Rc::from("Nil")] => Type::Nil,
                smallvec![Rc::from("I8")] => Type::I8,
                smallvec![Rc::from("I32")] => Type::I32,
                smallvec![Rc::from("I64")] => Type::I64,
                smallvec![Rc::from("ISize")] => Type::ISize,
                smallvec![Rc::from("F64")] => Type::F64,
                smallvec![Rc::from("Substring")] => Type::I8.dyn_slice(),
            ],
            builtins: Builtins {
                structs: fnv_hashmap![],
                generic_fat_pointer_struct: Rc::new(generic_fat_pointer_struct),
            },
        }
    }
}

pub struct SSAVisitor<'a, 'b> {
    context: Context,
    envs: Vec<Env>,
    top_level: &'a OptCell<TopLevelInfo>,
    ast_top_level: &'a RefCell<AstTopLevelInfo>,
    has_direct_return: bool,
    has_break: bool,
    last_retvar: Option<Variable>,
    sources: &'a RefCell<&'b mut Sources>,
    current_path: Vec<Rc<str>>,
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

impl<'a, 'b> SSAVisitor<'a, 'b> {
    pub fn generate(
        program: &mut ast::Program,
        sources: &'a RefCell<&'b mut Sources>,
    ) -> Result<isa::Program, compiler::Error> {
        let top_level = OptCell::some(TopLevelInfo::new());
        let ast_top_level = RefCell::new(AstTopLevelInfo::new());
        let mut visitor = SSAVisitor::new(&top_level, &ast_top_level, sources);
        match visitor.visit_program(program) {
            Ok(_) => Ok(visitor.into_program()),
            Err(e) => Err(e),
        }
    }

    fn new(
        top_level: &'a OptCell<TopLevelInfo>,
        ast_top_level: &'a RefCell<AstTopLevelInfo>,
        sources: &'a RefCell<&'b mut Sources>,
    ) -> Self {
        Self {
            context: Context::new(Rc::from(runtime::MAIN_NAME)),
            envs: Vec::with_capacity(4),
            top_level,
            ast_top_level,
            has_direct_return: false,
            has_break: false,
            last_retvar: None,
            sources,
            current_path: vec![],
        }
    }

    fn derive_with_context(&mut self, context: Context) -> Self {
        Self {
            context,
            envs: Vec::with_capacity(4),
            top_level: self.top_level,
            ast_top_level: self.ast_top_level,
            has_direct_return: false,
            has_break: false,
            last_retvar: None,
            sources: self.sources,
            current_path: vec![],
        }
    }

    fn into_context(self) -> Context {
        self.context
    }

    fn into_program(self) -> isa::Program {
        let top_level = self.top_level.take().into_inner().unwrap();
        let context = self.context;
        let mut func_contexts = top_level.func_contexts;
        let entry = Rc::new(FunctionName {
            path: smallvec![context.name.clone()],
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
            if let Some(var) = env.get_var(var) {
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
        let intrinsic: &IntrinsicType = &defstmt.func_attrs.intrinsic.borrow();
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
            let func_attrs = &defstmt.func_attrs;
            for attr in attrs {
                match attr.name.as_ref() {
                    "Static" => {
                        func_attrs
                            .intrinsic
                            .replace(IntrinsicType::Extern(attr.args[0].to_string()));
                    }
                    "Public" => {
                        func_attrs.always_generate.set(true);
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

    fn insert_cast(&mut self, left_var: Variable, mut typed: Type, location: u32) -> Variable {
        if self.context.variable(left_var) == &typed {
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
                location,
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
                .unwrap_or_else(|| self.context.variable(left_var))
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
                    let location = self.location_for(idx);
                    let mut idx_var = self.last_retvar.take().unwrap();
                    assert!(self.context.variable(idx_var).is_int());
                    idx_var = self.insert_cast(idx_var, Type::ISize, location);
                    let typed = last_typed.instance_type().unwrap().clone();
                    indices.push(MemberExprIndex {
                        var: MemberExprIndexVar::Variable(idx_var),
                        typed,
                    });
                }
            }
        }
        let last_typed = &indices.last().unwrap().typed;
        let modifier = if last_typed.is_copyable() {
            ReferenceModifier::Copy
        } else {
            ReferenceModifier::Move
        };
        Ok(InsType::MemberReference {
            left: left_var,
            indices: indices.into_boxed_slice(),
            modifier,
        })
    }

    fn get_struct_type(&self, typed: &Type) -> Option<Rc<StructType>> {
        match typed {
            maybe_ptr if maybe_ptr.is_fat_pointer() => {
                let top_level = self.top_level.borrow().unwrap();
                Some(top_level.builtins.generic_fat_pointer_struct.clone())
            }
            Type::Struct(struct_type) => Some(struct_type.clone().into()),
            _ => None,
        }
    }

    fn forward_decl_class(
        &mut self,
        name: &PathVec,
    ) -> Result<Option<UniqueRc<StructType>>, compiler::Error> {
        if let Some(boxed) = self.ast_top_level.borrow().class_stmts.get(name).cloned() {
            let class_stmt = boxed.borrow().downcast_ref::<ClassStatement>().unwrap();
            class_stmt.actual_path.replace(name.clone());
            self.visit_class(class_stmt, &boxed)?;
            let top_level = self.top_level.borrow().unwrap();
            Ok(top_level.builtins.structs.get(name).cloned())
        } else {
            Ok(None)
        }
    }

    fn location_for(&mut self, boxed: &NodeBox) -> u32 {
        let mut sources = self.sources.borrow_mut();
        sources.insert_span(boxed.span())
    }

    fn finish_env(&mut self) -> Option<Env> {
        if let Some(mut env) = self.envs.pop() {
            if env.var_stack.is_empty() {
                return Some(env);
            }

            let block = self.context.blocks.last_mut().unwrap();
            debug_assert!(block.ins.last().unwrap().typed.is_jmp());

            let postlude = block.ins.pop().unwrap();
            let mut var_stack = std::mem::replace(&mut env.var_stack, vec![]);
            postlude.each_used_var(|var| {
                var_stack.remove_item(&var);
            });
            for &var in var_stack.iter().rev() {
                if self.context.variables[usize::from(var)].is_value_type() {
                    block.ins.push(Ins::empty_ret(InsType::Drop(var), 0));
                }
            }
            block.ins.push(postlude);

            Some(env)
        } else {
            None
        }
    }

    fn insert_drop(&mut self, var: Variable) {
        self.with_block_mut(|block| {
            block.ins.push(Ins::empty_ret(InsType::Drop(var), 0));
        });
    }

    fn with_current_path(&self, name: Rc<str>) -> PathVec {
        let mut path: SmallVec<_> = self.current_path.clone().into();
        path.push(name);
        path
    }

    fn generate_call(
        &mut self,
        mut func_name: FunctionName,
        mut args: Vec<Variable>,
        mut arg_types: Vec<Type>,
        b: &NodeBox,
    ) -> Result<Variable, compiler::Error> {
        let location = self.location_for(b);
        let mut func_name_rc = None;
        let rettype = {
            let top_level = self.top_level.borrow_mut().unwrap();
            if let Some((key, maybe_context)) = top_level.func_contexts.get_key_value(&func_name) {
                if let Some(context) = maybe_context {
                    func_name_rc = Some(key.clone());
                    Some(context.rettype.clone())
                } else {
                    None
                }
            } else if self
                .ast_top_level
                .borrow()
                .defstmts
                .contains_key(&func_name.path)
            {
                None
            } else {
                return Err(
                    Error::UnknownIdentifier(func_name.path.last().cloned().unwrap())
                        .into_compiler_error(b),
                );
            }
        };

        let rettype = if let Some(rettype) = rettype {
            rettype
        } else {
            let (defstmt, func_insert) = {
                let mut top_level = self.top_level.borrow_mut().unwrap();
                let mut usable_defstmt = None;
                if let Some(vec) = self.ast_top_level.borrow().defstmts.get(&func_name.path) {
                    for boxed in vec {
                        let defstmt = boxed.rc().downcast_rc::<DefStatement>().unwrap();
                        match defstmt.compatibility_with_args(&arg_types) {
                            ArgCompatibility::None => (),
                            ArgCompatibility::WithCast(casts) => {
                                for (idx, typed) in casts {
                                    let var = args[idx];
                                    let retvar = self.insert_cast(var, typed.clone(), location);

                                    args[idx] = retvar;
                                    arg_types[idx] = typed.clone();
                                    func_name.arg_types[idx] = typed.clone();
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
                let func_insert = if let Some((func_name_rc_old, _)) =
                    top_level.func_contexts.get_key_value(&func_name)
                {
                    func_name_rc = Some(func_name_rc_old.clone());
                    true
                } else {
                    func_name_rc = Some(Rc::new(func_name));
                    top_level
                        .func_contexts
                        .insert(func_name_rc.clone().unwrap(), None);
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
                if !defstmt.func_attrs.intrinsic.borrow().is_none() {
                    if let Some(rettype) = SSAVisitor::intrinsic_return_type(&defstmt) {
                        let rc = func_name_rc.clone().unwrap();
                        let context = Context::with_intrinsics(
                            rc.path.last().cloned().unwrap(),
                            arg_types,
                            rettype.clone(),
                            defstmt.func_attrs.intrinsic.clone().into_inner(),
                        );
                        let mut top_level = self.top_level.borrow_mut().unwrap();
                        top_level.func_contexts.insert(rc, Some(context));
                        rettype
                    } else {
                        unimplemented!()
                    }
                } else {
                    let rc = func_name_rc.clone().unwrap();
                    let context = {
                        let func_context =
                            Context::with_args(rc.path.last().cloned().unwrap(), arg_types);
                        let mut visitor = self.derive_with_context(func_context);
                        visitor.visit_defstmt(defstmt.borrow(), b)?;
                        visitor.into_context()
                    };
                    let mut top_level = self.top_level.borrow_mut().unwrap();
                    let rettype = context.rettype.clone();
                    top_level.func_contexts.insert(rc, Some(context));
                    rettype
                }
            }
        };
        let retvar = self.context.insert_var(rettype);
        {
            let name = self.context.call_name(func_name_rc.clone().unwrap());
            let block = self.context.block_mut();
            block.ins.push(Ins::new(
                retvar,
                InsType::Call {
                    name,
                    args: args.clone().into_boxed_slice(),
                },
                location,
            ));
            for arg in &args {
                if !self.context.variable(*arg).is_copyable() {
                    let block = self.context.block_mut();
                    block
                        .ins
                        .push(Ins::empty_ret(InsType::MarkMoved(*arg), location));
                }
            }
        }
        Ok(retvar)
    }
}

impl<'a, 'b> Visitor for SSAVisitor<'a, 'b> {
    fn visit_program(&mut self, n: &mut ast::Program) -> VisitorResult {
        let mut top_level_stmts = vec![];
        let mut outerstmts = vec![];
        for expr in &n.exprs {
            if let Some(defstmt) = expr.borrow().downcast_ref::<DefStatement>() {
                let mut ast_top_level = self.ast_top_level.borrow_mut();
                if let Some(vec) = ast_top_level
                    .defstmts
                    .get_mut(&smallvec![defstmt.id.clone()])
                {
                    vec.push(expr.clone());
                } else {
                    ast_top_level
                        .defstmts
                        .insert(smallvec![defstmt.id.clone()], vec![expr.clone()]);
                }
            } else if let Some(class_stmt) = expr.borrow().downcast_ref::<ClassStatement>() {
                let mut ast_top_level = self.ast_top_level.borrow_mut();
                ast_top_level
                    .class_stmts
                    .insert(smallvec![class_stmt.id.clone()], expr.clone());
            } else if expr.borrow().downcast_ref::<ImportStatement>().is_some() {
                top_level_stmts.push(expr);
            } else if let Some(stmt) = expr.borrow().downcast_ref::<ModStatement>() {
                self.visit_modstmt(stmt, expr)?;
            } else {
                outerstmts.push(expr);
            }
        }

        for node in top_level_stmts {
            node.visit(self)?;
        }

        {
            let ast_top_level = self.ast_top_level.borrow();
            for defstmt_vec in ast_top_level.defstmts.values() {
                for boxed in defstmt_vec {
                    let defstmt: &DefStatement =
                        boxed.borrow().downcast_ref::<DefStatement>().unwrap();
                    self.top_level_visit_defstmt(&defstmt, &boxed)?;
                }
            }
        }

        self.context.new_block();
        self.envs.push(Env::new());
        for node in outerstmts {
            node.visit(self)?;
        }
        self.with_block_mut(|block| {
            block.ins.push(Ins::empty_ret(InsType::Exit, 0));
        });
        self.finish_env().unwrap();

        {
            let ast_top_level = self.ast_top_level.borrow();
            for defstmt_vec in ast_top_level.defstmts.values() {
                for boxed in defstmt_vec {
                    let defstmt: &DefStatement =
                        boxed.borrow().downcast_ref::<DefStatement>().unwrap();

                    if !defstmt.func_attrs.always_generate.get() {
                        continue;
                    }

                    let mut top_level = self.top_level.borrow_mut().unwrap();
                    let mut arg_types = vec![];
                    for arg in &defstmt.args {
                        arg_types.push(
                            arg.type_id
                                .as_ref()
                                .unwrap()
                                .typed
                                .borrow()
                                .as_ref()
                                .unwrap()
                                .clone(),
                        );
                    }
                    let id = FunctionName {
                        path: smallvec![defstmt.id.clone()],
                        arg_types: arg_types.clone(),
                    };
                    if top_level
                        .func_contexts
                        .get(&id)
                        .map(|x| x.is_some())
                        .unwrap_or(false)
                    {
                        continue;
                    }
                    let id = Rc::new(id);
                    top_level.func_contexts.insert(id.clone(), None);
                    std::mem::drop(top_level);

                    let func_context = Context::with_args(defstmt.id.clone(), arg_types);
                    let mut visitor = self.derive_with_context(func_context);
                    visitor.visit_defstmt(defstmt, boxed)?;
                    let context = visitor.into_context();
                    let mut top_level = self.top_level.borrow_mut().unwrap();
                    top_level.func_contexts.insert(id, Some(context));
                }
            }
        }

        Ok(())
    }

    fn visit_import(&mut self, _n: &ImportStatement, _b: &NodeBox) -> VisitorResult {
        Ok(())
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
            let mut top_level = self.top_level.borrow_mut().unwrap();
            let struct_rc = UniqueRc::new(struct_type);
            top_level
                .builtins
                .structs
                .insert(n.actual_path.borrow().clone(), struct_rc.clone());
            top_level
                .types
                .insert(n.actual_path.borrow().clone(), Type::Struct(struct_rc));
        }
        Ok(())
    }

    fn visit_class_init(&mut self, n: &ClassInitExpr, b: &NodeBox) -> VisitorResult {
        let struct_data = if let Some(struct_data) = self
            .top_level
            .borrow()
            .map(|top_level| top_level.builtins.structs.get(&n.path).cloned())
            .flatten()
        {
            struct_data
        } else if let Some(struct_data) = self.forward_decl_class(&n.path)? {
            struct_data
        } else {
            return Err(Error::UnknownType(n.path.last().cloned().unwrap()).into_compiler_error(b));
        };
        let location = self.location_for(b);
        let retvar = self.context.insert_var(Type::Struct(struct_data.clone()));
        self.with_block_mut(|block| {
            block
                .ins
                .push(Ins::new(retvar, InsType::LoadStruct, location));
        });
        for (name, expr) in &n.inits {
            expr.visit(self)?;
            let location = self.location_for(expr);
            let right = self.last_retvar.take().unwrap();
            if let Some(field) = struct_data.vars().get(name) {
                if self.context.variable(right) != &field.typed {
                    return Err(Error::IncompatibleType {
                        got: self.context.variable(right).clone(),
                        expected: field.typed.clone(),
                    }
                    .into_compiler_error(b));
                }
                let copyable = self.context.variable(right).is_copyable();
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(
                        retvar,
                        InsType::MemberReferenceStore {
                            indices: vec![MemberExprIndex {
                                var: MemberExprIndexVar::StructIndex(field.idx),
                                typed: field.typed.clone(),
                            }]
                            .into_boxed_slice(),
                            modifier: if copyable {
                                ReferenceModifier::Copy
                            } else {
                                ReferenceModifier::Move
                            },
                            right,
                        },
                        location,
                    ));
                });
            } else {
                unimplemented!()
            }
        }
        self.last_retvar = Some(retvar);
        Ok(())
    }

    fn visit_modstmt(&mut self, n: &ModStatement, _b: &NodeBox) -> VisitorResult {
        self.current_path.push(n.id.clone());
        for expr in &n.exprs {
            if let Some(defstmt) = expr.borrow().downcast_ref::<DefStatement>() {
                let path = self.with_current_path(defstmt.id.clone());
                let mut ast_top_level = self.ast_top_level.borrow_mut();
                if let Some(vec) = ast_top_level.defstmts.get_mut(&path) {
                    vec.push(expr.clone());
                } else {
                    ast_top_level.defstmts.insert(path, vec![expr.clone()]);
                }
            } else if let Some(stmt) = expr.borrow().downcast_ref::<ClassStatement>() {
                let path = self.with_current_path(stmt.id.clone());
                dbg_println!("{:?}", path);
                let mut ast_top_level = self.ast_top_level.borrow_mut();
                ast_top_level.class_stmts.insert(path, expr.clone());
            } else {
                unreachable!()
            }
        }
        self.current_path.pop();
        Ok(())
    }

    fn visit_defstmt(&mut self, n: &DefStatement, b: &NodeBox) -> VisitorResult {
        {
            let mut env = Env::new();
            for (idx, def_argument) in n.args.iter().enumerate() {
                env.insert_var(
                    def_argument.name.clone(),
                    env::Variable {
                        var: Variable::with_u32(idx as u32),
                        is_mut: def_argument.is_mut,
                    },
                );
            }
            self.envs.push(env);
        }
        let location = self.location_for(b);
        if n.exprs.is_empty() {
            self.context.rettype = Type::Nil;
            let retvar = self.context.insert_var(Type::Nil);
            self.with_block_mut(|block| {
                block.ins.extend_from_slice(&[
                    Ins::new(retvar, InsType::LoadNil, location),
                    Ins::empty_ret(InsType::Return(retvar), location),
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
                self.context.rettype = self.context.rettype.unify(self.context.variable(retvar));
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::Return(retvar), location));
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
                        Ins::new(retvar, InsType::LoadNil, location),
                        Ins::empty_ret(InsType::Return(retvar), location),
                    ]);
                });
            } else {
                self.context.rettype = self.context.rettype.unify(&Type::Nil);
                let retvar = self.context.insert_var(Type::Nil);
                self.with_block_mut(|block| {
                    block.ins.extend_from_slice(&[
                        Ins::new(retvar, InsType::LoadNil, location),
                        Ins::empty_ret(InsType::Return(retvar), location),
                    ]);
                });
            }
        }
        if let Some(declared_return) = declared_return {
            if declared_return != self.context.rettype {
                return Err(Error::InvalidReturnType.into_compiler_error(b));
            }
        }
        self.finish_env().unwrap();
        Ok(())
    }

    fn visit_return(&mut self, n: &ReturnExpr, b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        if let Some(retvar) = self.last_retvar.take() {
            let rettype = self.context.variable(retvar).clone();
            if self.context.rettype == Type::NoReturn {
                self.context.rettype = rettype;
            } else if rettype != self.context.rettype {
                self.context.rettype = self.context.rettype.unify(&rettype);
            }
            let location = self.location_for(b);
            self.with_block_mut(|block| {
                block
                    .ins
                    .push(Ins::new(retvar, InsType::Return(retvar), location));
            });
        }
        self.has_direct_return = true;
        Ok(())
    }

    fn visit_whileexpr(&mut self, n: &WhileExpr, b: &NodeBox) -> VisitorResult {
        let cond_block;
        let while_block;
        let mut while_retvar = None;
        let location = self.location_for(b);

        self.envs.push(Env::new_breakable());
        check_cf_state!(self, {
            cond_block = Some(self.context.new_block());
            n.cond.visit(self)?;
            let cond_location = self.location_for(&n.cond);
            if let Some(last_retvar) = self.last_retvar.take() {
                self.with_block_mut(|block| {
                    block.ins.push(Ins::empty_ret(
                        InsType::IfJmp {
                            condvar: last_retvar,
                            iftrue: cond_block.unwrap(),
                            iffalse: 0usize,
                        },
                        cond_location,
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
                        .push(Ins::empty_ret(InsType::Jmp(cond_block.unwrap()), location));
                });

                let new_block = self.context.new_block();
                let retvar = self.context.insert_var(Type::Nil);
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(retvar, InsType::LoadNil, location));
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
                            .push(Ins::empty_ret(InsType::Jmp(cond_block.unwrap()), location));
                    });
                }
            }
        });
        let env = self.finish_env().unwrap();

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
        for InsPosition { block_idx, ins_idx } in env.break_idx.unwrap() {
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
        let location = self.location_for(b);

        // If-true branch
        self.envs.push(Env::new());
        let (treturn, tbreak) = check_cf_state!(self, {
            n.cond.visit(self)?;
            cond = self.context.blocks.len() - 1;
            if let Some(last_retvar) = self.last_retvar.take() {
                condvar = last_retvar;
                if self.context.variable(last_retvar) != &Type::Bool {
                    return Err(Error::IncompatibleType {
                        got: self.context.variable(last_retvar).clone(),
                        expected: Type::Bool,
                    }
                    .into_compiler_error(b));
                }
            } else {
                // condition returns
                self.finish_env().unwrap();
                return Ok(());
            }
            if !n.exprs.is_empty() {
                iftrue_start = Some(self.context.new_block());
                for node in &n.exprs {
                    node.visit(self)?;
                    propagate_cf_state!(self);
                }
                iftrue_end = Some(self.context.blocks.len() - 1);
                if let Some(retvar) = self.last_retvar.take() {
                    iftrue_retvar = Some(retvar);
                }
            }
        });
        self.finish_env().unwrap();

        if n.exprs.is_empty() && n.elses.is_empty() {
            // Return nil if two branches don't do anything
            let retvar = self.context.insert_var(Type::Nil);
            self.with_block_mut(|block| {
                block.ins.push(Ins::new(retvar, InsType::LoadNil, location));
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
                if let Some(retvar) = self.last_retvar.take() {
                    iffalse_retvar = Some(retvar);
                }
            }
        });
        self.finish_env().unwrap();

        if treturn && treturn == freturn {
            self.has_direct_return = true;
            return Ok(());
        }

        if tbreak && tbreak == fbreak {
            self.has_break = true;
            return Ok(());
        }

        let retvar_type = if b.generate_retvar.get() {
            match (iftrue_retvar, iffalse_retvar) {
                (Some(first), Some(last)) => Some(
                    self.context
                        .variable(first)
                        .unify(self.context.variable(last)),
                ),
                (Some(first), None) => Some(self.context.variable(first).unify(&Type::Nil)),
                (None, Some(last)) => Some(self.context.variable(last).unify(&Type::Nil)),
                (None, None) => None,
            }
        } else {
            None
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
                let typed = &self.context.variables[usize::from(retvar)];
                if typed.is_nil() {
                    block.ins.push(Ins::new(retvar, InsType::LoadNil, location));
                } else if typed.is_memory_type() {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadStruct, location));
                }
            }
            if !self.has_direct_return {
                block.ins.push(Ins::empty_ret(
                    InsType::IfJmp {
                        condvar,
                        iftrue: iftrue_start.or(outer_block).unwrap(),
                        iffalse: iffalse_start.or(outer_block).unwrap(),
                    },
                    location,
                ));
            }
        }
        if let Some(iftrue_end) = iftrue_end {
            let block = &mut self.context.blocks[iftrue_end];
            if let Some(retvar) = retvar {
                // FIXME: this should be a separate function
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                let typed = &self.context.variables[usize::from(retvar)];
                if typed.is_memory_type() {
                    block.ins.push(Ins::empty_ret(
                        InsType::Store {
                            source: last_retvar,
                            dest: retvar,
                        },
                        location,
                    ));
                    if !self.context.variables[usize::from(last_retvar)].is_copyable() {
                        block
                            .ins
                            .push(Ins::empty_ret(InsType::MarkMoved(last_retvar), location));
                    }
                } else if typed.is_copyable() {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::Copy(last_retvar), location));
                } else {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::Move(last_retvar), location));
                }
            }
            if let Some(outer_block) = outer_block {
                block
                    .ins
                    .push(Ins::empty_ret(InsType::Jmp(outer_block), location));
            }
        }
        if let Some(iffalse_end) = iffalse_end {
            let block = &mut self.context.blocks[iffalse_end];
            if let Some(retvar) = retvar {
                let last_retvar = block.ins.last().unwrap().retvar().unwrap();
                // FIXME: this should be a separate function
                let typed = &self.context.variables[usize::from(retvar)];
                if typed.is_memory_type() {
                    block.ins.push(Ins::empty_ret(
                        InsType::Store {
                            source: last_retvar,
                            dest: retvar,
                        },
                        location,
                    ));
                    if !self.context.variables[usize::from(last_retvar)].is_copyable() {
                        block
                            .ins
                            .push(Ins::empty_ret(InsType::MarkMoved(last_retvar), location));
                    }
                } else if typed.is_copyable() {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::Copy(last_retvar), location));
                } else {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::Move(last_retvar), location));
                }
            }
            if let Some(outer_block) = outer_block {
                block
                    .ins
                    .push(Ins::empty_ret(InsType::Jmp(outer_block), location));
            }
        }

        self.last_retvar = retvar;
        Ok(())
    }

    fn visit_callexpr(&mut self, n: &CallExpr, b: &NodeBox) -> VisitorResult {
        let mut args = Vec::with_capacity(n.args.len());
        let mut arg_types = Vec::with_capacity(n.args.len());
        for arg in &n.args {
            arg.visit(self)?;
            let retvar = self.last_retvar.take().unwrap();
            args.push(retvar);
            arg_types.push(self.context.variable(retvar).clone());
        }

        let borrowed = n.callee.borrow();
        if let Some(id) = borrowed
            .downcast_ref::<Value>()
            .map(|val| val.as_identifier())
            .flatten()
        {
            let func_name = FunctionName {
                path: self.with_current_path(id.clone()),
                arg_types: arg_types.clone(),
            };
            let retvar = self.generate_call(func_name, args, arg_types, b)?;
            if b.generate_retvar.get() {
                self.last_retvar = Some(retvar);
            } else {
                self.insert_drop(retvar);
            }
            Ok(())
        } else if let Some(n) = borrowed.downcast_ref::<PathExpr>() {
            let func_name = FunctionName {
                path: n.path.clone(),
                arg_types: arg_types.clone(),
            };
            let retvar = self.generate_call(func_name, args, arg_types, b)?;
            if b.generate_retvar.get() {
                self.last_retvar = Some(retvar);
            } else {
                self.insert_drop(retvar);
            }
            Ok(())
        } else {
            Err(Error::InvalidLHS.into_compiler_error(b))
        }
    }

    fn visit_letexpr(&mut self, n: &LetExpr, b: &NodeBox) -> VisitorResult {
        let location = self.location_for(b);
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
                                        block.ins.push(Ins::new(
                                            retvar,
                                            InsType::LoadSlice(vec![].into_boxed_slice()),
                                            location,
                                        ));
                                    });
                                }
                                _ => unimplemented!(),
                            }
                            let env = self.envs.last_mut().unwrap();
                            env.insert_var(
                                id.clone(),
                                env::Variable {
                                    var: retvar,
                                    is_mut: n.is_mut,
                                },
                            );
                            if b.generate_retvar.get() {
                                self.last_retvar = Some(retvar);
                            }
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
                    .insert_var(self.context.variable(right).clone());
                if self.context.variable(right).is_copyable() {
                    self.with_block_mut(|block| {
                        block
                            .ins
                            .push(Ins::new(var, InsType::Copy(right), location));
                    });
                } else {
                    self.with_block_mut(|block| {
                        block
                            .ins
                            .push(Ins::new(var, InsType::Move(right), location));
                    });
                }
                let env = self.envs.last_mut().unwrap();
                env.insert_var(
                    id.clone(),
                    env::Variable {
                        var,
                        is_mut: n.is_mut,
                    },
                );
                if b.generate_retvar.get() {
                    self.last_retvar = Some(right);
                }
                return Ok(());
            }
        }
        Err(Error::InvalidLHS.into_compiler_error(b))
    }

    fn visit_binexpr(&mut self, n: &BinExpr, b: &NodeBox) -> VisitorResult {
        let location = self.location_for(b);
        match &n.op {
            BinOp::Asg | BinOp::Adds | BinOp::Subs | BinOp::Muls | BinOp::Divs | BinOp::Mods => {
                let n_left = n.left.borrow();
                if let Some(id) = n_left
                    .downcast_ref::<ast::Value>()
                    .map(|v| v.as_identifier())
                    .flatten()
                {
                    let var = if let Some(env_var) = self.get_var(&id) {
                        if !env_var.is_mut {
                            return Err(Error::MutatingImmutable(id.clone()).into_compiler_error(b));
                        }
                        env_var.var
                    } else {
                        return Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b));
                    };
                    n.right.visit(self)?;
                    let right = self.last_retvar.take().unwrap();
                    if self.context.variable(var) != self.context.variable(right) {
                        return Err(Error::IncompatibleType {
                            got: self.context.variable(right).clone(),
                            expected: self.context.variable(var).clone(),
                        }
                        .into_compiler_error(b));
                    }
                    match &n.op {
                        BinOp::Asg => {
                            let copyable = self.context.variable(var).is_copyable();
                            self.with_block_mut(|block| {
                                if copyable {
                                    block
                                        .ins
                                        .push(Ins::new(var, InsType::Copy(right), location));
                                } else {
                                    block
                                        .ins
                                        .push(Ins::new(var, InsType::Move(right), location));
                                }
                            });
                        }
                        BinOp::Adds => {
                            self.with_block_mut(|block| {
                                block
                                    .ins
                                    .push(Ins::new(var, InsType::Add((var, right)), location));
                            });
                        }
                        BinOp::Subs => {
                            self.with_block_mut(|block| {
                                block
                                    .ins
                                    .push(Ins::new(var, InsType::Sub((var, right)), location));
                            });
                        }
                        BinOp::Muls => {
                            self.with_block_mut(|block| {
                                block
                                    .ins
                                    .push(Ins::new(var, InsType::Mul((var, right)), location));
                            });
                        }
                        BinOp::Divs => {
                            self.with_block_mut(|block| {
                                block
                                    .ins
                                    .push(Ins::new(var, InsType::Div((var, right)), location));
                            });
                        }
                        BinOp::Mods => {
                            self.with_block_mut(|block| {
                                block
                                    .ins
                                    .push(Ins::new(var, InsType::Mod((var, right)), location));
                            });
                        }
                        _ => unreachable!(),
                    }
                    if b.generate_retvar.get() {
                        self.last_retvar = Some(var);
                    }
                    Ok(())
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
                                        left,
                                        InsType::MemberReferenceStore {
                                            indices: std::mem::replace(
                                                &mut indices,
                                                vec![].into_boxed_slice(),
                                            ),
                                            modifier,
                                            right,
                                        },
                                        location,
                                    ));
                                });
                            }
                            _ => unimplemented!(),
                        }
                        if b.generate_retvar.get() {
                            self.last_retvar = Some(right);
                        }
                        Ok(())
                    } else {
                        unreachable!()
                    }
                } else if let Some(deref_expr) = n_left.downcast_ref::<ast::DerefExpr>() {
                    let inner_expr = deref_expr.expr.borrow();
                    n.right.visit(self)?;
                    let right = self.last_retvar.take().unwrap();
                    if let Some(id) = inner_expr
                        .downcast_ref::<ast::Value>()
                        .map(|v| v.as_identifier())
                        .flatten()
                    {
                        let var = if let Some(env_var) = self.get_var(&id) {
                            if !self.context.variable(env_var.var).is_mut_pointer() {
                                return Err(
                                    Error::MutatingImmutable(id.clone()).into_compiler_error(b)
                                );
                            }
                            env_var.var
                        } else {
                            return Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b));
                        };
                        match &n.op {
                            BinOp::Asg => {
                                self.with_block_mut(|block| {
                                    block.ins.push(Ins::empty_ret(
                                        InsType::Store {
                                            source: right,
                                            dest: var,
                                        },
                                        location,
                                    ));
                                });
                            }
                            _ => {
                                let typed = self.context.variable(var).clone();
                                let old = self.context.insert_var(typed);
                                self.with_block_mut(|block| {
                                    block.ins.push(Ins::new(old, InsType::Load(var), location));
                                });
                                self.with_block_mut(|block| {
                                    block.ins.push(Ins::new(
                                        old,
                                        match &n.op {
                                            BinOp::Adds => InsType::Add((old, right)),
                                            BinOp::Subs => InsType::Sub((old, right)),
                                            BinOp::Muls => InsType::Mul((old, right)),
                                            BinOp::Divs => InsType::Div((old, right)),
                                            BinOp::Mods => InsType::Mod((old, right)),
                                            _ => unreachable!(),
                                        },
                                        location,
                                    ));
                                    block.ins.push(Ins::empty_ret(
                                        InsType::Store {
                                            source: right,
                                            dest: var,
                                        },
                                        location,
                                    ));
                                });
                            }
                        }
                        Ok(())
                    } else {
                        Err(Error::InvalidLHS.into_compiler_error(&n.left))
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
                    block
                        .ins
                        .push(Ins::new(result, InsType::Move(left), location));
                }

                let (right_block_start, right_block_end);
                {
                    right_block_start = self.context.new_block();
                    n.right.visit(self)?;
                    let right = self.last_retvar.take().unwrap();

                    right_block_end = self.context.new_block();
                    let block = &mut self.context.blocks[right_block_end];
                    block
                        .ins
                        .push(Ins::new(result, InsType::Move(right), location));
                }

                // Insert retvar
                let end_block = self.context.new_block();

                // Insert jumps
                {
                    let block = &mut self.context.blocks[left_block_end];
                    match &n.op {
                        BinOp::And => block.ins.push(Ins::empty_ret(
                            InsType::IfJmp {
                                condvar: result,
                                iftrue: right_block_start,
                                iffalse: end_block,
                            },
                            location,
                        )),
                        BinOp::Or => block.ins.push(Ins::empty_ret(
                            InsType::IfJmp {
                                condvar: result,
                                iftrue: end_block,
                                iffalse: right_block_start,
                            },
                            location,
                        )),
                        _ => unreachable!(),
                    }
                }
                {
                    let block = &mut self.context.blocks[right_block_end];
                    block
                        .ins
                        .push(Ins::empty_ret(InsType::Jmp(end_block), location));
                }

                if b.generate_retvar.get() {
                    self.last_retvar = Some(result);
                } else {
                    self.insert_drop(result);
                }
                Ok(())
            }
            op => {
                n.left.visit(self)?;
                let left = self.last_retvar.take().unwrap();
                n.right.visit(self)?;
                let mut right = self.last_retvar.take().unwrap();
                if self.context.variable(left) != self.context.variable(right) {
                    let left_typed = self.context.variable(left).clone();
                    if self
                        .context
                        .variable(right)
                        .can_implicit_cast_to(&left_typed)
                    {
                        right = self.insert_cast(right, left_typed, location);
                    } else {
                        return Err(Error::IncompatibleType {
                            got: self.context.variable(right).clone(),
                            expected: self.context.variable(left).clone(),
                        }
                        .into_compiler_error(b));
                    }
                }
                let retvar = self.context.insert_var(match op {
                    BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Equ => Type::Bool,
                    _ => self.context.variable(left).clone(),
                });
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(
                        retvar,
                        {
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
                        },
                        location,
                    ));
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                } else {
                    self.insert_drop(retvar);
                }
                Ok(())
            }
        }
    }

    fn visit_asexpr(&mut self, n: &AsExpr, b: &NodeBox) -> VisitorResult {
        n.left.visit(self)?;
        let retvar = self.last_retvar.take().unwrap();
        n.typed.visit(self, b)?;
        let rettype = n.typed.typed.borrow().clone().unwrap();
        let location = self.location_for(b);
        self.last_retvar = Some(self.insert_cast(retvar, rettype, location));
        Ok(())
    }

    fn visit_member_expr(&mut self, n: &MemberExpr, b: &NodeBox) -> VisitorResult {
        let location = self.location_for(b);
        if let InsType::MemberReference {
            left,
            mut indices,
            modifier,
        } = self.build_member_expr(n, b)?
        {
            let typed = indices.as_ref().last().unwrap().typed.clone();
            let retvar = self.context.insert_var(typed);
            self.with_block_mut(move |block| {
                block.ins.push(Ins::new(
                    retvar,
                    InsType::MemberReference {
                        left,
                        indices: std::mem::replace(&mut indices, vec![].into_boxed_slice()),
                        modifier,
                    },
                    location,
                ));
            });
            if b.generate_retvar.get() {
                self.last_retvar = Some(retvar);
            }
            Ok(())
        } else {
            unreachable!()
        }
    }

    fn visit_value(&mut self, n: &Value, b: &NodeBox) -> VisitorResult {
        let location = self.location_for(b);
        match n {
            Value::Uninitialized => unimplemented!(),
            Value::I32(x) => {
                let retvar = self.context.insert_var(Type::I32);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadI32(*x as i32), location));
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                } else {
                    self.insert_drop(retvar);
                }
                Ok(())
            }
            Value::I64(x) => {
                let retvar = self.context.insert_var(Type::I64);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadI64(*x), location));
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                } else {
                    self.insert_drop(retvar);
                }
                Ok(())
            }
            Value::ISize(x) => {
                let retvar = self.context.insert_var(Type::ISize);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadI64(*x), location))
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                } else {
                    self.insert_drop(retvar);
                }
                Ok(())
            }
            Value::Float(x) => {
                let retvar = self.context.insert_var(Type::F64);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadF64(*x), location));
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                } else {
                    self.insert_drop(retvar);
                }
                Ok(())
            }
            Value::Bool(x) => {
                let retvar = self.context.insert_var(Type::Bool);
                self.with_block_mut(|block| {
                    block
                        .ins
                        .push(Ins::new(retvar, InsType::LoadBool(*x), location));
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                } else {
                    self.insert_drop(retvar);
                }
                Ok(())
            }
            Value::String(x) => {
                let retvar = self
                    .context
                    .insert_var(Type::I8.dyn_slice().pointer(BorrowModifier::Immutable));
                self.with_block_mut(|block| {
                    block.ins.push(Ins::new(
                        retvar,
                        InsType::LoadSubstring(x.clone()),
                        location,
                    ));
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(retvar);
                }
                Ok(())
            }
            Value::Identifier(id) => {
                if let Some(env_var) = self.get_var(&id) {
                    if b.generate_retvar.get() {
                        self.last_retvar = Some(env_var.var);
                    }
                    Ok(())
                } else {
                    Err(Error::UnknownIdentifier(id.clone()).into_compiler_error(b))
                }
            }
            Value::Slice(slice) => {
                let mut typed = None;
                let mut retvars = Vec::with_capacity(slice.len());
                for item in slice {
                    item.visit(self)?;
                    let retvar = self.last_retvar.unwrap();
                    if let Some(old_typed) = typed {
                        typed = Some(self.context.variable(retvar).unify(&old_typed));
                    } else {
                        typed = Some(self.context.variable(retvar).clone());
                    }
                    retvars.push(retvar);
                }
                if let Some(typed) = typed {
                    let copyable = typed.is_copyable();
                    let retvar = self.context.insert_var(typed.slice(slice.len() as u32));
                    let location = self.location_for(b);
                    self.with_block_mut(|block| {
                        let retvars = std::mem::replace(&mut retvars, vec![]);
                        block.ins.push(Ins::new(
                            retvar,
                            InsType::LoadSlice(retvars.clone().into_boxed_slice()),
                            location,
                        ));
                        if !copyable {
                            for retvar in &retvars {
                                block
                                    .ins
                                    .push(Ins::empty_ret(InsType::MarkMoved(*retvar), location));
                            }
                        }
                    });
                    if b.generate_retvar.get() {
                        self.last_retvar = Some(retvar);
                    } else {
                        self.insert_drop(retvar);
                    }
                    Ok(())
                } else {
                    Err(Error::CannotInfer.into_compiler_error(b))
                }
            }
        }
    }

    fn visit_typeid(&mut self, n: &TypeId, b: &NodeBox) -> VisitorResult {
        match &n.data {
            TypeIdData::Path(path) => {
                if n.typed.borrow().is_some() {
                    Ok(())
                } else if let Some(typed) = self
                    .top_level
                    .borrow()
                    .map(|top_level| top_level.types.get(path).cloned())
                    .flatten()
                {
                    n.typed.replace(Some(typed));
                    Ok(())
                } else if let Some(struct_data) = self.forward_decl_class(path)? {
                    n.typed.replace(Some(Type::Struct(struct_data)));
                    Ok(())
                } else {
                    Err(Error::UnknownType(path.last().cloned().unwrap()).into_compiler_error(b))
                }
            }
            TypeIdData::Pointer {
                typed: internal,
                borrow_mod,
            } => {
                self.visit_typeid(&internal, b)?;
                n.typed.replace(Some(
                    internal
                        .typed
                        .borrow()
                        .clone()
                        .unwrap()
                        .pointer(*borrow_mod),
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

    fn visit_break(&mut self, _n: &BreakExpr, b: &NodeBox) -> VisitorResult {
        self.has_break = true;
        let block_idx = self.context.blocks.len() - 1;
        let location = self.location_for(b);
        let ins_idx = self.with_block_mut(|block| {
            let idx = block.ins.len();
            block.ins.push(Ins::empty_ret(InsType::Jmp(0), location));
            idx
        });
        let env = self.get_breakable().unwrap();
        env.break_idx
            .as_mut()
            .unwrap()
            .push(InsPosition { block_idx, ins_idx });
        Ok(())
    }

    fn visit_borrow(&mut self, n: &BorrowExpr, b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        let location = self.location_for(b);
        let expr = self.last_retvar.take().unwrap();
        let expr_typed = self.context.variable(expr).clone();
        let retvar = self.context.insert_var(expr_typed.pointer(n.borrow_mod));
        self.with_block_mut(|block| {
            block.ins.push(Ins::new(
                retvar,
                InsType::Borrow {
                    var: expr,
                    modifier: n.borrow_mod,
                },
                location,
            ));
        });
        if b.generate_retvar.get() {
            self.last_retvar = Some(retvar);
        }
        Ok(())
    }

    fn visit_deref(&mut self, n: &DerefExpr, b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        let location = self.location_for(b);
        let expr = self.last_retvar.take().unwrap();
        if let Some(typed) = self.context.variable(expr).instance_type().cloned() {
            let retvar = self.context.insert_var(typed);
            self.with_block_mut(|block| {
                block
                    .ins
                    .push(Ins::new(retvar, InsType::Load(expr), location));
            });
            self.last_retvar = Some(retvar);
            Ok(())
        } else {
            Err(Error::CannotDeref.into_compiler_error(b))
        }
    }

    fn visit_unary(&mut self, n: &UnaryExpr, b: &NodeBox) -> VisitorResult {
        n.expr.visit(self)?;
        let retvar = self.last_retvar.take().unwrap();
        match n.op {
            UnaryOp::Uni => {
                let new_retvar = self.context.insert_var(
                    self.context
                        .variable(retvar)
                        .clone()
                        .pointer(BorrowModifier::Unique),
                );
                let location = self.location_for(b);
                self.with_block_mut(|block| {
                    block.ins.extend_from_slice(&[
                        Ins::new(new_retvar, InsType::AllocHeap, location),
                        Ins::empty_ret(
                            InsType::Store {
                                source: retvar,
                                dest: new_retvar,
                            },
                            location,
                        ),
                        Ins::new(new_retvar, InsType::MarkMoved(retvar), location),
                    ]);
                });
                if b.generate_retvar.get() {
                    self.last_retvar = Some(new_retvar);
                } else {
                    self.insert_drop(retvar);
                }
            }
        }
        Ok(())
    }

    fn visit_path(&mut self, _n: &PathExpr, _b: &NodeBox) -> VisitorResult {
        unimplemented!()
    }
}
