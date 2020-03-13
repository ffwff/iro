use std::borrow::{Borrow, BorrowMut};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use crate::ast::*;
use crate::ast;
use crate::types::types::*;
use crate::env::env::Env;

pub struct TypeVisitor {
    envs: Vec<Env>,
    funcs: HashMap<Rc<str>, Function>,
    types: HashMap<Rc<str>, TypeInfo>,
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
                "String".into() => TypeInfo::new_with_type(Type::String),
                "Integer".into() => TypeInfo::new_with_type(Type::Integer),
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

    fn getvar(&self, s : &str) -> Option<Variable> {
        for scope in self.envs.iter().rev() {
            if let Some(var) = scope.getvar(s) {
                return Some(var)
            }
        }
        None
    }

    fn cur_function(&self) -> Option<Function> {
        if let Some(scope) = self.envs.first() {
            if let Some(function) = &scope.function {
                return Some(function.clone())
            }
        }
        None
    }

    fn copy_unresolved(dest : &Unresolved, src : TypeInfo) {
        let urd_rc : &RefCell<UnresolveData> = dest.borrow();
        let urd : &UnresolveData = &urd_rc.borrow();
        let var_rcc : Variable = urd.id.clone().unwrap().upgrade().unwrap();
        let var_rc : &RefCell<VariableData> = var_rcc.borrow();
        let var : &mut VariableData = &mut var_rc.borrow_mut();
        var.type_info = src.clone();
    }

    fn derive_overload(fdata: &mut FunctionData, args: &Vec<NodeBox>) -> Result<TypeInfo, Error> {
        let mut overload =
            if fdata.overloads.is_some() {
                Some(FunctionOverload::new())
            } else {
                None
            };
        for ((_, var), boxed) in fdata.args.iter().zip(args) {
            // println!("{:#?} {:#?}", var, boxed);
            let vdata_rc : &RefCell<VariableData> = var.borrow();
            let derive_caller =
                if let Type::Unresolved(_) = &boxed.type_info().borrow().typed() {
                    true
                } else {
                    false
                };
            let vdata : &VariableData = &vdata_rc.borrow();
            if derive_caller {
                if let Type::Unresolved(_) = vdata.type_info.typed() {
                    // The two pairs are unresolved, can't do anything here!
                    return Err(Error::CannotInfer);
                } else {
                    // Infer caller argument's type if the function already has a type
                    if let Type::Unresolved(resolved) = &boxed.type_info().borrow().typed() {
                        TypeVisitor::copy_unresolved(resolved, vdata.type_info.clone())
                    }
                    boxed.type_info().replace(vdata.type_info.clone());
                }
            } else  {
                let type_info : &TypeInfo = &boxed.type_info().borrow();
                if let Type::Unresolved(_) = &vdata.type_info.typed() {
                    let overload = overload.as_mut().unwrap();
                    overload.args.push(type_info.clone());
                    continue;
                } else if &vdata.type_info != type_info {
                    return Err(Error::IncompatibleType);
                }
            }
            if let Some(overload) = overload.as_mut() {
                overload.args.push(vdata.type_info.clone());
            }
        }
        let returntype = if let Type::Unresolved(unresolved) = fdata.returntype.typed() {
            // Try to infer return type from function arguments
            let mut returntype : Option<TypeInfo> = None;
            for (idx, (_, var)) in fdata.args.iter().enumerate() {
                let vdata_rc : &RefCell<VariableData> = var.borrow();
                let vdata : &VariableData = &vdata_rc.borrow();
                if let Type::Unresolved(other) = &vdata.type_info.typed() {
                    if unresolved == other {
                        returntype = Some(args[idx].type_info().borrow().clone());
                        break;
                    }
                }
            }
            if let Some(returntype) = returntype {
                if let Some(overload) = overload.as_mut() {
                    overload.returntype = returntype.clone();
                }
                returntype
            } else {
                return Err(Error::InvalidArguments);
            }
        } else {
            fdata.returntype.clone()
        };
        if let Some(overload) = overload {
            fdata.overloads.as_mut().unwrap().insert(overload);
        }
        Ok(returntype)
    }
}

impl<'a> Visitor for TypeVisitor {
    fn visit_program(&mut self, n: &Program) -> VisitorResult {
        for node in &n.exprs {
            if let Some(def) = node.downcast_ref::<DefStatement>() {
                // Populate environment with arguments
                self.enter_scope();
                let mut args = Vec::new(); 
                {
                    for (id, type_id) in &def.args {
                        if let Some(type_id) = type_id {
                            type_id.node().visit(&type_id, self)?;
                            let var = Rc::new(RefCell::new(VariableData::new_with_type(type_id.type_info().borrow().clone())));
                            self.scope().setvar(id.clone(), var.clone());
                            args.push((id.to_string(), var));
                        } else {
                            let var = self.scope().defvar_unresolved(id.clone());
                            args.push((id.to_string(), var));
                        }
                    }
                }
                let function = Rc::new(RefCell::new(FunctionData::new(args)));
                self.scope().function = Some(function.clone());
                
                // Visit inner function statements
                let mut has_direct_return = false;
                for child in &def.exprs {
                    child.visit(&child, self)?;
                    if let Some(ifexpr) = child.downcast_ref::<IfExpr>() {
                        if ifexpr.returntype.get() == IfReturnType::BothBranch {
                            has_direct_return = true;
                            break;
                        }
                    } else if let Some(_) = child.downcast_ref::<ReturnExpr>() {
                        has_direct_return = true;
                        break;
                    }
                }

                // Leave the function!
                self.leave_scope();
                node.type_info().replace(TypeInfo::new_with_type(Type::Function(function.clone())));
                {
                    let fdata_rc : &RefCell<FunctionData> = function.borrow();
                    let fdata : &mut FunctionData = &mut fdata_rc.borrow_mut();
                    if !has_direct_return {
                        fdata.returntype.add_type(Type::Nil);
                    }
                    fdata.check_overloads();
                }
                self.funcs.insert(def.id.clone(), function.clone());
            }
        }
        
        self.enter_scope();
        for node in &n.exprs {
            if let None = node.downcast_ref::<DefStatement>() {
                node.visit(&node, self)?;
            }
        }
        self.leave_scope();
        Ok(())
    }

    fn visit_defstmt(&mut self, _b: &NodeBox, _n: &DefStatement) -> VisitorResult {
        Err(Error::InternalError)
    }

    fn visit_return(&mut self, b: &NodeBox, n: &ReturnExpr) -> VisitorResult {
        self.has_unbranched_return = true;
        n.expr.visit(&n.expr, self)?;
        let type_info = n.expr.type_info().borrow();
        b.type_info().replace(type_info.clone());
        {
            let cur_function = self.cur_function().unwrap();
            let fdata_rc : &RefCell<FunctionData> = cur_function.borrow();
            let fdata : &mut FunctionData = &mut fdata_rc.borrow_mut();
            fdata.returntype.add_type(type_info.typed().clone());
        }
        Ok(())
    }

    fn visit_ifexpr(&mut self, b: &NodeBox, n: &IfExpr) -> VisitorResult {
        // cond + If-true branch
        self.enter_scope();
        let treturn = check_unbranched_return!(self, {
            n.cond.visit(&n.cond, self)?;
            for node in &n.exprs {
                node.visit(&node, self)?;
            }
        });
        let tscope = self.leave_scope().unwrap();

        // If-false branch
        self.enter_scope();
        let freturn = check_unbranched_return!(self, {
            for node in &n.elses {
                node.visit(&node, self)?;
            }
        });
        let mut fscope = self.leave_scope().unwrap();

        // Combine two scopes and put it in the current one
        let curscope = self.scope();
        for (id, var) in tscope.vars() {
            curscope.setvar(id.clone(), var.clone());
            let var_b : &RefCell<VariableData> = var.borrow();
            let var_t : &mut TypeInfo = &mut var_b.borrow_mut().type_info;
            let has_var = if let Some(altvar) = fscope.vars().get(id) {
                let altvar_b : &RefCell<VariableData> = altvar.borrow();
                let altvar_t : &TypeInfo = &altvar_b.borrow().type_info;
                var_t.add_type(altvar_t.typed().clone());
                true
            } else {
                var_t.add_type(Type::Nil);
                false
            };
            if has_var {
                fscope.mut_vars().remove(id);
            }
        }

        // Skip type inference if the statement only has a condition
        if n.exprs.is_empty() && n.elses.is_empty() {
            b.type_info().replace(TypeInfo::new_with_type(Type::Nil));
        } else {
            for (id, var) in fscope.vars() {
                curscope.setvar(id.clone(), var.clone());
                let var_b : &RefCell<VariableData> = var.borrow();
                let var_t : &mut TypeInfo = &mut var_b.borrow_mut().type_info;
                if let Some(altvar) = tscope.vars().get(id) {
                    let altvar_b : &RefCell<VariableData> = altvar.borrow();
                    let altvar_t : &TypeInfo = &altvar_b.borrow().type_info;
                    var_t.add_type(altvar_t.typed().clone());
                } else {
                    var_t.add_type(Type::Nil);
                }
            }

            // Combine return values for 2 branches
            let mut retval = TypeInfo::new();
            if let Some(last) = n.exprs.last() {
                let type_info : &TypeInfo = &last.type_info().borrow();
                retval.add_type(type_info.typed().clone());
            } else {
                retval.add_type(Type::Nil);
            }
            if let Some(last) = n.elses.last() {
                let type_info : &TypeInfo = &last.type_info().borrow();
                retval.add_type(type_info.typed().clone());
            } else {
                retval.add_type(Type::Nil);
            }
            b.type_info().replace(retval);
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
            node.visit(&node, self)?;
        }
        if let Some(value) = n.callee.downcast_ref::<Value>() {
            if let Value::Identifier(var) = &value {
                if let Some(func) = self.funcs.get(&var.id) {
                    let fdata_rc : &RefCell<FunctionData> = func.borrow();
                    let fdata : &mut FunctionData = &mut fdata_rc.borrow_mut();
                    if n.args.len() != fdata.args.len() {
                        return Err(Error::NotEnoughArguments);
                    }
                    let returntype = TypeVisitor::derive_overload(fdata, &n.args)?;
                    b.type_info().replace(returntype);
                    return Ok(())
                } else {
                    return Err(Error::UnknownIdentifier(var.id.clone()))
                }
            }
        }
        Err(Error::InvalidLHS)
    }

    fn visit_binexpr(&mut self, b : &NodeBox, n: &BinExpr) -> VisitorResult {
        match n.op {
            BinOp::Asg => {
                let node = n.left.downcast_ref::<Value>();
                match &node {
                    Some(Value::Identifier(var)) => {
                        n.right.visit(&n.right, self)?;
                        let right_type = n.right.type_info().clone().into_inner();
                        match self.getvar(&var.id) {
                            Some(mut curvar) => {
                                curvar.borrow_mut().replace(VariableData::new_with_type(right_type));
                                n.left.type_info().replace(TypeInfo::new_with_type(Type::Identifier(curvar)));
                            }
                            None => {
                                let mut curvar = self.scope().defvar(var.id.clone());
                                curvar.borrow_mut().replace(VariableData::new_with_type(right_type));
                                n.left.type_info().replace(TypeInfo::new_with_type(Type::Identifier(curvar)));
                            }
                        }
                        b.type_info().replace(n.right.type_info().borrow().clone());
                    }
                    _ => return Err(ast::Error::InvalidLHS),
                }
            }
            _ => {
                n.left.visit(&n.left, self)?;
                n.right.visit(&n.right, self)?;
                let left = n.left.type_info().borrow().typed().clone();
                let right = n.right.type_info().borrow().typed().clone();
                // Two branches of a binaryexpr must be the same
                // If they are both undefined, prefer the leftmost branch
                match (&left, &right) {
                    (Type::Unresolved(_), Type::Unresolved(rightv)) => {
                        let leftmost = n.left.type_info().borrow();
                        n.right.type_info().replace(leftmost.clone());
                        TypeVisitor::copy_unresolved(&rightv, leftmost.clone());
                        b.type_info().replace(leftmost.clone());
                    },
                    (Type::Unresolved(leftv), _) => {
                        let typeinfo = n.right.type_info().borrow();
                        n.left.type_info().replace(typeinfo.clone());
                        TypeVisitor::copy_unresolved(&leftv, typeinfo.clone());
                        b.type_info().replace(typeinfo.clone());
                    },
                    (_, Type::Unresolved(rightv)) => {
                        let typeinfo = n.left.type_info().borrow();
                        n.right.type_info().replace(typeinfo.clone());
                        TypeVisitor::copy_unresolved(&rightv, typeinfo.clone());
                        b.type_info().replace(typeinfo.clone());
                    },
                    (ta, tb) => {
                        if ta != tb {
                            return Err(ast::Error::IncompatibleType)
                        }
                        b.type_info().replace(TypeInfo::new_with_type(left));
                    }
                }
            }
        }
        Ok(())
    }

    fn visit_value(&mut self, b : &NodeBox, n: &Value) -> VisitorResult {
        let info = b.type_info();
        match n {
            Value::Integer(_) => {
                info.replace(TypeInfo::new_with_type(Type::Integer));
            }
            Value::Float(_) => {
                info.replace(TypeInfo::new_with_type(Type::Float));
            }
            Value::String(_) => {
                info.replace(TypeInfo::new_with_type(Type::String));
            }
            Value::Identifier(var) => {
                if let Some(curvar) = self.getvar(&var.id) {
                    var.var.replace(Some(curvar.clone()));
                    info.replace(TypeInfo::from_variable(&curvar));
                } else {
                    return Err(ast::Error::UnknownIdentifier(var.id.clone()))
                }
            }
            _ => return Err(ast::Error::InternalError),
        }
        Ok(())
    }

    fn visit_typeid(&mut self, b: &NodeBox, n: &TypeId) -> VisitorResult {
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