use std::rc::{Rc, Weak};
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::{HashSet, HashMap};
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use crate::types::types::Type;

thread_local! {
    static UNTYPED: TypeInfo = TypeInfo::new_typed(Type::Untyped);
    static NIL_TYPED: TypeInfo = TypeInfo::new_typed(Type::Nil);
}

pub struct TypeInfo {
    typed: Rc<RefCell<Type>>,
}

impl TypeInfo {
    pub fn new() -> Self {
        UNTYPED.with(|untyped| {
            untyped.clone()
        })
    }

    pub fn new_nil() -> Self {
        NIL_TYPED.with(|nil_typed| {
            nil_typed.clone()
        })
    }

    pub fn new_typed(typed : Type) -> Self {
        TypeInfo {
            typed: Rc::new(RefCell::new(typed)),
        }
    }

    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.typed)
    }

    pub fn typed(&self) -> &Rc<RefCell<Type>> {
        &self.typed
    }

    pub fn unionize(&self, other: &TypeInfo) -> Option<Self> {
        if self.ptr_eq(other) {
            return None
        }
        self.with(|typed| {
            other.with(|other_typed| {
                match (&typed, &other_typed) {
                    (Type::Untyped, _) => {
                        Some(other.clone())
                    }
                    (Type::Union(set), _) => {
                        let mut new = set.clone();
                        if other_typed == &Type::Unresolved {
                            new.insert(Type::UnresolvedRedirect(other.clone()));
                        } else {
                            new.insert(other_typed.clone());
                        }
                        Some(TypeInfo::new_typed(Type::Union(new)))
                    }
                    (_, Type::Union(other_set)) => {
                        let mut new = other_set.clone();
                        if typed == &Type::Unresolved {
                            new.insert(Type::UnresolvedRedirect(self.clone()));
                        } else {
                            new.insert(typed.clone());
                        }
                        Some(TypeInfo::new_typed(Type::Union(new)))
                    }
                    (_, _) => {
                        if typed == other_typed {
                            return None;
                        }
                        let mut new = HashSet::new();
                        if typed == &Type::Unresolved {
                            new.insert(Type::UnresolvedRedirect(self.clone()));
                        } else {
                            new.insert(typed.clone());
                        }
                        if other_typed == &Type::Unresolved {
                            new.insert(Type::UnresolvedRedirect(other.clone()));
                        } else {
                            new.insert(other_typed.clone());
                        }
                        Some(TypeInfo::new_typed(Type::Union(new)))
                    }
                }
            })
        })
    }
    
    pub fn is_untyped(&self) -> bool {
        self.with(|typed| {
            match typed {
                Type::Untyped => true,
                _ => false
            }
        })
    }
    
    pub fn is_hard_unresolved(&self) -> bool {
        self.with(|typed| {
            match typed {
                Type::Unresolved => true,
                _ => false
            }
        })
    }
    
    pub fn is_soft_unresolved(&self) -> bool {
        self.with(|typed| {
            match typed {
                Type::UnresolvedRedirect(_) => true,
                _ => false
            }
        })
    }

    pub fn is_unresolved(&self) -> bool {
        self.is_soft_unresolved() || self.is_hard_unresolved()
    }
    
    pub fn is_function(&self) -> bool {
        self.with(|typed| {
            match typed {
                Type::Function(_) => true,
                _ => false
            }
        })
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.typed, &other.typed)
    }

    pub fn resolve(&self, other: &TypeInfo) {
        let typed_rc : &RefCell<Type> = self.typed.borrow();
        other.with(|typed| {
            typed_rc.replace(typed.clone())
        });
    }
    
    pub fn resolve_typed(&self, other: Type) {
        let typed_rc : &RefCell<Type> = self.typed.borrow();
        typed_rc.replace(other);
    }

    pub fn with<T, U>(&self, mut callback: T) -> U where T : FnMut(&Type) -> U {
        let typed_rc : &RefCell<Type> = self.typed.borrow();
        let typed : &Type = &typed_rc.borrow();
        let retval = callback(typed);
        retval
    }

    pub fn with_mut<T, U>(&self, mut callback: T) -> U where T : FnMut(&mut Type) -> U {
        let typed_rc : &RefCell<Type> = self.typed.borrow();
        let typed : &mut Type = &mut typed_rc.borrow_mut();
        let retval = callback(typed);
        retval
    }
}

impl std::cmp::PartialEq for TypeInfo {
    fn eq(&self, other: &Self) -> bool {
        if self.is_soft_unresolved() && other.is_hard_unresolved() {
            self.with(|typed| {
                match typed {
                    Type::UnresolvedRedirect(x) => x.ptr_eq(other),
                    _ => unreachable!()
                }
            })
        } else if self.is_soft_unresolved() && other.is_hard_unresolved() {
            other.with(|typed| {
                match typed {
                    Type::UnresolvedRedirect(x) => x.ptr_eq(self),
                    _ => unreachable!()
                }
            })
        } else if self.is_hard_unresolved() && other.is_hard_unresolved() {
            self.ptr_eq(other)
        } else {
            self.typed == other.typed
        }
    }
}

impl std::cmp::Eq for TypeInfo {}

impl std::hash::Hash for TypeInfo {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.with(|typed| {
            typed.hash(state)
        });
    }
}

impl std::fmt::Debug for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#?} ({:p})", self.typed, self.typed)
    }
}

impl std::clone::Clone for TypeInfo {
    fn clone(&self) -> Self {
        if self.is_soft_unresolved() {
            self.with(|typed| {
                match typed {
                    Type::UnresolvedRedirect(redirect) => redirect.clone(),
                    _ => unreachable!()
                }
            })
        } else {
            Self {
                typed: self.typed.clone()
            }
        }
    }
}