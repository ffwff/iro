use std::borrow::{Borrow, BorrowMut};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::ast::*;
use crate::ast;
use crate::types;
use crate::env::env::Env;
use crate::env::var::*;
use crate::types::resolve_visitor::ResolveVisitor;

pub struct TypeVisitor {
    envs: Vec<Env>,
    funcs: HashMap<Rc<str>, types::Function>,
    types: HashMap<Rc<str>, types::TypeInfo>,
    has_unbranched_return: bool,
}

macro_rules! check_unbranched_return {
    ($self:expr, $x:block) => {{
        let _has_unbranched_return = $self.has_unbranched_return;
        $self.has_unbranched_return = false;
        $x
        let _retval = $self.has_unbranched_return;
        $self.has_unbranched_return = _has_unbranched_return;
        _retval
    }};
}

impl TypeVisitor {
    pub fn new() -> Self {
        TypeVisitor {
            envs: vec![],
            funcs: HashMap::new(),
            types: hashmap!{
                "String".into() => types::TypeInfo::new_typed(types::Type::String),
                "Int".into()    => types::TypeInfo::new_typed(types::Type::Int),
            },
            has_unbranched_return: false,
        }
    }

    fn enter_scope(&mut self) {
        self.envs.push(Env::new());
    }

    fn leave_scope(&mut self) -> Option<Env> {
        self.envs.pop()
    }
    
    fn scope(&mut self) -> &mut Env {
        self.envs.last_mut().unwrap()
    }

    fn getvar(&self, s : &str) -> Option<&Variable> {
        for scope in self.envs.iter().rev() {
            if let Some(var) = scope.vars().get(s) {
                return Some(var)
            }
        }
        None
    }

    fn cur_function(&self) -> Option<types::Function> {
        if let Some(scope) = self.envs.first() {
            if let Some(function) = &scope.function {
                return Some(function.clone())
            }
        }
        None
    }
}

impl<'a> Visitor for TypeVisitor {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult {
        let mut defstmts = vec![];
        let mut regstmts = vec![];
        for node in &n.exprs {
            if node.downcast_ref::<DefStatement>().is_some() {
                defstmts.push(node);
            } else {
                regstmts.push(node);
            }
        }

        // Function definition statements
        for node in &defstmts {
            let def = node.node();
            node.visit(self)?;
        }


        // Regular top-level statements
        self.enter_scope();
        for node in &regstmts {
            node.visit(self)?;
        }
        self.leave_scope();

        // Calculate unresolved function arguments
        for node in &defstmts {
            if {
                let borrow = node.type_info().borrow();
                borrow.is_function()
            } {
                let mut visitor = ResolveVisitor::new(node);
                visitor.process();
            }
        }

        Ok(())
    }

    fn visit_defstmt(&mut self,  b: &NodeBox, n: &DefStatement) -> VisitorResult {
        self.enter_scope();
        let mut fargs = vec![];
        for (id, arg) in &n.args {
            // FIXME: prespecified argument typing
            if let Some(arg) = arg {
                arg.visit(self);
            } else {
                let type_info = types::TypeInfo::new_typed(types::Type::Unresolved);
                fargs.push((id.clone(), type_info.clone()));
                self.scope().vars_mut().insert(id.clone(), Variable::new(VariableData::new(type_info)));
            }
        }

        let func = types::Function::new(types::FunctionData::new(fargs));
        self.scope().function = Some(func.clone());

        self.has_unbranched_return = false;
        for expr in &n.exprs {
            expr.visit(self)?;
            if self.has_unbranched_return {
                break;
            }
        }

        self.leave_scope();

        func.with_mut(|func| {
            if func.returntype.is_untyped() {
                func.returntype.resolve_typed(types::Type::Nil);
            } else if !self.has_unbranched_return {
                if let Some(newtype) = func.returntype.unionize(&types::TypeInfo::new_nil()) {
                    func.returntype = newtype;
                }
            }
            func.check_overloads();
        });

        self.funcs.insert(n.id.clone(), func.clone());
        b.type_info().replace(types::TypeInfo::new_typed(types::Type::Function(func)));
        Ok(())
    }

    fn visit_return(&mut self,   b: &NodeBox, n: &ReturnExpr) -> VisitorResult {
        self.has_unbranched_return = true;
        n.expr.visit(self)?;
        b.type_info().replace(n.expr.type_info().borrow().clone());
        {
            let cur_function = self.cur_function().unwrap();
            cur_function.with_mut(|function| {
                if let Some(newtype) = function.returntype.unionize(&n.expr.type_info().borrow()) {
                    function.returntype = newtype;
                }
            })
        }
        Ok(())
    }

    fn visit_ifexpr(&mut self,  b: &NodeBox, n: &IfExpr) -> VisitorResult {
        // cond + If-true branch
        self.enter_scope();
        let treturn = check_unbranched_return!(self, {
            n.cond.visit(self)?;
            for node in &n.exprs {
                node.visit(self)?;
            }
        });
        let tscope = self.leave_scope().unwrap();

        // If-false branch
        self.enter_scope();
        let freturn = check_unbranched_return!(self, {
            for node in &n.elses {
                node.visit(self)?;
            }
        });
        let mut fscope = self.leave_scope().unwrap();

        if n.exprs.is_empty() && n.elses.is_empty() {
            // Skip type inference if the statement only has a condition
            b.type_info().replace(types::TypeInfo::new_typed(types::Type::Nil));
        } else {
            // Do type inference for leaking variables
            // If-true scope
            for (id, var) in tscope.vars() {
                var.with(|var| {
                    let mut typed = var.typed.clone();
                    if let Some(altvar) = fscope.vars().get(id) {
                        altvar.with(|altvar| {
                            if let Some(newtype) = typed.unionize(&altvar.typed) {
                                typed = newtype;
                            }
                        })
                    } else if let Some(newtype) = typed.unionize(&types::TypeInfo::new_nil()) {
                        typed = newtype;
                    }
                    let newvar = Variable::new(VariableData::new(typed));
                    self.scope().vars_mut().insert(id.clone(), newvar);
                });
            }
            // If-false scope
            for (id, var) in fscope.vars() {
                let var_rc : &Rc<RefCell<VariableData>> = var.inner();
                let var_rcc : &RefCell<VariableData> = &var_rc;
                let var : &VariableData = &var_rcc.borrow();
                if let Some(altvar) = tscope.vars().get(id) {
                    break;
                }
                let mut typed = var.typed.clone();
                let newvar = Variable::new(VariableData::new(typed));
                self.scope().vars_mut().insert(id.clone(), newvar);
            }

            // Combine return values for 2 branches
            if n.exprs.is_empty() {
                b.type_info().replace(n.elses.last().unwrap().type_info().borrow().clone());
            } else if n.elses.is_empty() {
                b.type_info().replace(n.exprs.last().unwrap().type_info().borrow().clone());
            } else {
                // Both branches are full
                let mut retval = n.exprs.last().unwrap().type_info().borrow().clone();
                println!("{:#?}", retval);
                if let Some(new) = retval.unionize(&n.elses.last().unwrap().type_info().borrow()) {
                    retval = new;
                }
                b.type_info().replace(retval);
            }
        }

        if treturn || freturn {
            if treturn != freturn {
                n.returntype.replace(IfReturnType::OneBranch);
            } else {
                n.returntype.replace(IfReturnType::BothBranch);
                self.has_unbranched_return = true;
            }
        }

        Ok(())
    }

    fn visit_callexpr(&mut self, b: &NodeBox, n: &CallExpr) -> VisitorResult {
        for node in &n.args {
            node.visit(self)?;
        }
        if let Some(value) = n.callee.downcast_ref::<Value>() {
            if let Value::Identifier(var) = &value {
                println!("{:#?} {:#?}", var.id, self.funcs);
                if let Some(func) = self.funcs.get(&var.id) {
                    n.function.replace(Some(func.clone()));
                    func.with_mut(|func| {
                        if func.returntype.is_unresolved() {
                            let mut types = vec![];
                            for node in &n.args {
                                types.push(node.type_info().borrow().clone());
                            }
                            if let Ok(returntype) = func.add_overload(types) {
                                b.type_info().replace(returntype);
                            } else {
                                unreachable!();
                            }
                        } else {
                            b.type_info().replace(func.returntype.clone());
                        }
                    });
                    return Ok(());
                } else {
                    // Forward declare the function
                    unimplemented!()
                }
            }
        }
        Err(Error::InvalidLHS)
    }

    fn visit_binexpr(&mut self,  b: &NodeBox, n: &BinExpr) -> VisitorResult {
        match n.op {
            BinOp::Asg => {
                let node = n.left.downcast_ref::<Value>();
                match &node {
                    Some(Value::Identifier(var)) => {
                        n.right.visit(self)?;
                        self.scope().vars_mut().insert(var.id.clone(),
                            Variable::new(VariableData::new(n.right.type_info().borrow().clone())));
                        b.type_info().replace(n.right.type_info().borrow().clone());
                        Ok(())
                    }
                    _ => return Err(ast::Error::InvalidLHS),
                }
            }
            _ => {
                n.left.visit(self)?;
                n.right.visit(self)?;
                match ({
                    let left = n.left.type_info().borrow();
                    left.is_hard_unresolved()
                },
                {
                    let right = n.right.type_info().borrow();
                    right.is_hard_unresolved()
                }) {
                    (true, true) => {
                        // Infer from leftmost branch
                        let ptr_eq = n.left.type_info().borrow()
                            .ptr_eq(&n.right.type_info().borrow());
                        if ptr_eq {
                            return Ok(())
                        } else {
                            let left = n.left.type_info().borrow();
                            let old = n.right.type_info().replace(left.clone());
                            if old.strong_count() > 1 {
                                old.resolve_typed(
                                    types::Type::UnresolvedRedirect(
                                        left.clone()
                                    )
                                );
                            }
                            b.type_info().replace(left.clone());
                        }
                        Ok(())
                    },
                    (false, true) => {
                        // Infer from leftmost branch
                        let left = n.left.type_info().borrow();
                        n.right.type_info().borrow().resolve(&left);
                        b.type_info().replace(left.clone());
                        Ok(())
                    },
                    (true, false) => {
                        // Infer from rightmost branch
                        let right = n.right.type_info().borrow();
                        n.left.type_info().borrow().resolve(&right);
                        b.type_info().replace(right.clone());
                        Ok(())
                    },
                    (false, false) => {
                        if n.left.type_info() == n.right.type_info() {
                            b.type_info().replace(n.left.type_info().borrow().clone());
                            Ok(())
                        } else {
                            println!("{:#?}", n);
                            Err(ast::Error::IncompatibleType)
                        }
                    },
                }
            }
        }
    }
    
    fn visit_value(&mut self,    b: &NodeBox, n: &Value) -> VisitorResult {
        let info = b.type_info();
        match n {
            Value::Int(_) => {
                info.replace(types::TypeInfo::new_typed(types::Type::Int));
            }
            Value::Float(_) => {
                info.replace(types::TypeInfo::new_typed(types::Type::Float));
            }
            Value::String(_) => {
                info.replace(types::TypeInfo::new_typed(types::Type::String));
            }
            Value::Identifier(var) => {
                if let Some(curvar) = self.getvar(&var.id) {
                    var.var.replace(Some(curvar.clone()));
                    curvar.with(|curvar| {
                        info.replace(curvar.typed.clone());
                    });
                } else {
                    return Err(ast::Error::UnknownIdentifier(var.id.clone()))
                }
            }
            _ => return Err(ast::Error::InternalError),
        }
        Ok(())
    }

    fn visit_typeid(&mut self,   b: &NodeBox, n: &TypeId) -> VisitorResult {
         match &n {
            TypeId::Identifier(s) => {
                if let Some(type_info) = self.types.get(s) {
                    b.type_info().replace(type_info.clone());
                } else {
                    return Err(ast::Error::UnknownIdentifier(s.clone()))
                }
            }
        }
        Ok(())
    }
}