use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub struct UniqueRc<T: ?Sized>(Rc<T>);

impl<T: Sized> UniqueRc<T> {
    pub fn new(data: T) -> Self {
        UniqueRc(Rc::new(data))
    }
}

impl<T: ?Sized> UniqueRc<T> {
    #[inline]
    fn as_ptr(&self) -> *const T {
        self.0.as_ref() as *const T
    }
}

impl<T: ?Sized> Deref for UniqueRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized> Hash for UniqueRc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state);
    }
}

impl<T: ?Sized> PartialEq for UniqueRc<T> {
    #[inline]
    fn eq(&self, other: &UniqueRc<T>) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    #[inline]
    fn ne(&self, other: &UniqueRc<T>) -> bool {
        !Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: ?Sized> Eq for UniqueRc<T> {}

impl<T: ?Sized> PartialOrd for UniqueRc<T> {
    #[inline]
    fn partial_cmp(&self, other: &UniqueRc<T>) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ?Sized> Ord for UniqueRc<T> {
    #[inline]
    fn cmp(&self, other: &UniqueRc<T>) -> Ordering {
        self.as_ptr().cmp(&other.as_ptr())
    }
}

impl<T> Into<UniqueRc<T>> for Rc<T> {
    fn into(self) -> UniqueRc<T> {
        UniqueRc(self)
    }
}

impl<T> Into<Rc<T>> for UniqueRc<T> {
    fn into(self) -> Rc<T> {
        self.0
    }
}

impl<T: ?Sized> AsRef<T> for UniqueRc<T> {
    fn as_ref(&self) -> &T {
        self.0.as_ref()
    }
}

impl<T: ?Sized> Borrow<T> for UniqueRc<T> {
    fn borrow(&self) -> &T {
        self.0.borrow()
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for UniqueRc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for UniqueRc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}
