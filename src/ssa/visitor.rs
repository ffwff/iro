use std::rc::Rc;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::borrow::Borrow;
use crate::ast;
use crate::ast::*;
use crate::ssa::isa::*;
use crate::ssa::env::Env;
use crate::utils::RcWrapper;

#[derive(Debug, Clone)]
pub struct TopLevelInfo {
    pub defstmts: HashMap<Rc<str>, Rc<DefStatement>>,
    pub func_contexts: HashMap<Rc<FunctionName>, Option<Context>>,
}

impl TopLevelInfo {
    pub fn new() -> Self {
        TopLevelInfo {
            defstmts: HashMap::new(),
            func_contexts: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct SSAVisitor {
    context: Context,
    envs: Vec<Env>,
    top_level: RcWrapper<TopLevelInfo>,
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

impl SSAVisitor {
    pub fn new() -> Self {
        Self {
            context: Context::new(Rc::from("main")),
            envs: vec![],
            top_level: RcWrapper::new(TopLevelInfo::new()),
            has_direct_return: false,
        }
    }

    pub fn with_context(context: Context, top_level: RcWrapper<TopLevelInfo>) -> Self {
        Self {
            context,
            envs: vec![],
            top_level,
            has_direct_return: false,
        }
    }

    pub fn into_context(self) -> Context {
        self.context
    }

    fn with_block<T, U>(&self, mut callback: T) -> U where T: FnMut(&Block) -> U {
        let block = self.context.block();
        callback(block)
    }

    fn with_block_mut<T, U>(&mut self, mut callback: T) -> U where T: FnMut(&mut Block) -> U {
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

    fn last_retvar(&self) -> usize {
        self.with_block(|block| block.ins.last().unwrap().retvar)
    }
}

impl Visitor for SSAVisitor {
    fn visit_program(&mut self,  n: &Program) -> VisitorResult {
        let mut outerstmts = vec![];
        for expr in &n.exprs {
            if let Some(defstmt) = expr.borrow().downcast_ref::<DefStatement>() {
                self.top_level.with_mut(|top_level| {
                    top_level.defstmts.insert(defstmt.id.clone(),
                            expr.rc().downcast_rc::<DefStatement>().unwrap());
                });
            } else {
                outerstmts.push(expr);
            }
        }

        self.context.new_block();
        self.envs.push(Env::new());
        for node in &outerstmts {
            node.visit(self)?;
        }
        Ok(())
    }

    fn visit_defstmt(&mut self,  n: &DefStatement) -> VisitorResult {
        {
            let mut env = Env::new();
            for (idx, (name, _)) in n.args.iter().enumerate() {
                env.vars_mut().insert(name.clone(), idx);
            }
            self.envs.push(env);
        }
        for expr in &n.exprs {
            expr.visit(self)?;
            if self.has_direct_return {
                break;
            }
        }
        if self.has_direct_return {
            let retvar = self.last_retvar();
            let rettype = self.context.variables[retvar].clone();
            self.context.rettype = rettype;
        } else {
            unimplemented!()
        }
        Ok(())
    }

    fn visit_return(&mut self,   n: &ReturnExpr) -> VisitorResult {
        n.expr.visit(self)?;
        let retvar = self.last_retvar();
        self.with_block_mut(|block| {
            block.ins.push(Ins { retvar, typed: InsType::Return(retvar) });
        });
        self.has_direct_return = true;
        Ok(())
    }

    fn visit_ifexpr(&mut self,   n: &IfExpr) -> VisitorResult {
        let mut cond = self.context.blocks.len() - 1;
        let mut condvar = 0usize;
        let mut iftrue =  None;
        let mut iffalse = None;

        // If-true branch
        self.envs.push(Env::new());
        let treturn = check_direct_return!(self, {
            n.cond.visit(self)?;
            condvar = self.last_retvar();
            if !n.exprs.is_empty() {
                iftrue = Some(self.context.new_block());
                for node in &n.exprs {
                    node.visit(self)?;
                }
            }
        });
        self.envs.pop();

        // If-false branch
        self.envs.push(Env::new());
        let freturn = check_direct_return!(self, {
            if !n.elses.is_empty() {
                iffalse = Some(self.context.new_block());
                for node in &n.elses {
                    node.visit(self)?;
                }
            }
        });
        self.envs.pop();

        // Insert jumps
        if iftrue.is_some() || iffalse.is_some() {
            self.context.new_block();
            let curblock = self.context.blocks.len() - 1;
            let block = &mut self.context.blocks[cond];
            block.ins.push(Ins { retvar: 0, typed: InsType::IfJmp((condvar,
                iftrue.unwrap_or(curblock),
                iffalse.unwrap_or(curblock))) });
            if iffalse.is_some() {
                if let Some(iftrue) = iftrue {
                    let block = &mut self.context.blocks[iftrue];
                    block.ins.push(Ins { retvar: 0, typed: InsType::Jmp(curblock) });
                }
            }
        }

        if treturn || freturn {
            if treturn != freturn {
                n.returntype.replace(IfReturnType::OneBranch);
            } else {
                n.returntype.replace(IfReturnType::BothBranch);
                self.has_direct_return = true;
            }
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
                    let retvar = self.last_retvar();
                    args.push(retvar);
                    arg_types.push(self.context.variables[retvar].clone());
                }
                let func_name = Rc::new(FunctionName {
                    name: id.clone(),
                    arg_types: arg_types.clone(),
                });

                let rettype = self.top_level.with_mut(|top_level| {
                    if let Some(maybe_context) = top_level.func_contexts.get(&func_name) {
                        if let Some(context) = maybe_context {
                            Ok(Some(context.rettype.clone()))
                        } else {
                            Ok(Some(Type::NoReturn))
                        }
                    } else if let Some(_) = top_level.defstmts.get(id) {
                        Ok(None)
                    } else {
                        Err(Error::UnknownIdentifier(id.clone()))
                    }
                })?;
                
                let retvar = self.context.insert_var(
                    if let Some(rettype) = rettype {
                        rettype
                    } else {
                        let visitor = RefCell::new(Some({
                            let func_context = Context::with_args(id.clone(), arg_types);
                            let mut visitor = SSAVisitor::with_context(func_context, self.top_level.clone());
                            let defstmt = self.top_level.with_mut(|top_level| {
                                top_level.func_contexts.insert(func_name.clone(), None);
                                top_level.defstmts.get(id).cloned().unwrap()
                            });
                            visitor.visit_defstmt(defstmt.borrow())?;
                            (func_name.clone(), visitor)
                        }));
                        self.top_level.with_mut(move |top_level| {
                            let (func_name, visitor) = visitor.replace(None).unwrap();
                            let context = visitor.into_context();
                            let rettype = context.rettype.clone();
                            top_level.func_contexts.insert(func_name, Some(context));
                            rettype
                        })
                    }
                );
                {
                    let args = RefCell::new(Some(args));
                    self.with_block_mut(|block| {
                        block.ins.push(Ins { retvar, typed: InsType::Call((
                                func_name.clone(),
                                args.replace(None).unwrap())) });
                    });
                }
                return Ok(())
            }
        }
        Err(Error::InvalidLHS)
        // if let Some(context) = self.func_contexts.get()
    }

    fn visit_letexpr(&mut self,  n: &LetExpr) -> VisitorResult {
        n.right.visit(self)?;
        let right = self.last_retvar();
        if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
            if let Value::Identifier(id) = &id {
                let env = self.envs.last_mut().unwrap();
                env.vars_mut().insert(id.clone(), right);
                return Ok(())
            }
        }
        Err(Error::InvalidLHS)
    }

    fn visit_binexpr(&mut self,  n: &BinExpr) -> VisitorResult {
        match &n.op {
            BinOp::Asg => {
                if let Some(id) = n.left.borrow().downcast_ref::<ast::Value>() {
                    if let Value::Identifier(id) = &id {
                        if let Some(var) = self.non_local(&id) {
                            let retvar = self.context.insert_var(self.context.variables[var].clone());
                            self.with_block_mut(|block| {
                                block.ins.push(Ins { retvar, typed: {
                                    InsType::LoadVar(var)
                                } });
                            });
                        } else {
                            return Err(Error::UnknownIdentifier(id.clone()));
                        }
                    }
                }
                Err(Error::InvalidLHS)
            },
            op => {
                n.left.visit(self)?;
                let left = self.last_retvar();
                n.right.visit(self)?;
                let right = self.last_retvar();
                if self.context.variables[left] != self.context.variables[right] {
                    return Err(Error::IncompatibleType);
                }
                let retvar = self.context.insert_var(self.context.variables[left].clone());
                self.with_block_mut(|block| {
                    block.ins.push(Ins { retvar, typed: {
                        match op {
                            BinOp::Add => InsType::Add((left, right)),
                            BinOp::Sub => InsType::Sub((left, right)),
                            BinOp::Mul => InsType::Mul((left, right)),
                            BinOp::Div => InsType::Div((left, right)),
                            _ => unimplemented!(),
                        }
                    } });
                });
                Ok(())
            }
        }
    }

    fn visit_value(&mut self,    n: &Value) -> VisitorResult {
        match n {
            Value::Int(x) => {
                let retvar = self.context.insert_var(Type::I32);
                self.with_block_mut(|block| {
                    block.ins.push(Ins { retvar, typed: InsType::LoadI32(*x) });
                });
                Ok(())
            }
            Value::String(x) => {
                let retvar = self.context.insert_var(Type::String);
                self.with_block_mut(|block| {
                    block.ins.push(Ins { retvar, typed: InsType::LoadString(x.clone()) });
                });
                Ok(())
            }
            Value::Identifier(id) => {
                if let Some(var) = self.non_local(&id) {
                    self.with_block_mut(|block| {
                        block.ins.push(Ins { retvar: var, typed: InsType::Nop });
                    });
                    Ok(())
                } else {
                    Err(Error::UnknownIdentifier(id.clone()))
                }
            }
            _ => unimplemented!()
        }
    }
    
    fn visit_typeid(&mut self,   n: &TypeId) -> VisitorResult {
        unimplemented!()
    }
}