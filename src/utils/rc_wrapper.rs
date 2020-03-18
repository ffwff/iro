use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RcWrapper<T> {
    inner: Rc<RefCell<T>>,
}

impl<T> RcWrapper<T> {
    pub fn new(data: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(data))
        }
    }

    pub fn as_ptr(&self) -> *const T {
        self.inner.as_ptr()
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }

    pub fn inner(&self) -> &Rc<RefCell<T>> {
        &self.inner
    }

    pub fn with<U, V>(&self, mut callback: U) -> V where U : FnMut(&T) -> V {
        let rc : &RefCell<T> = &self.inner;
        let data : &T = &rc.borrow();
        let retval = callback(data);
        retval
    }

    pub fn with_mut<U, V>(&self, mut callback: U) -> V where U : FnMut(&mut T) -> V {
        let rc : &RefCell<T> = &self.inner;
        let data : &mut T = &mut rc.borrow_mut();
        let retval = callback(data);
        retval
    }

    pub fn try_unwrap(self) -> Result<T, RcWrapper<T>> {
        match Rc::try_unwrap(self.inner) {
            Ok(rcc) => Ok(rcc.into_inner()),
            Err(inner) => Err(RcWrapper { inner }),
        }
    }
}