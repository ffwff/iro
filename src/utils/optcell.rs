use std::cell::{Ref, RefCell, RefMut, UnsafeCell};
use std::fmt;

pub struct OptCell<T> {
    data: UnsafeCell<Option<RefCell<T>>>,
}

impl<'a, T> OptCell<T> {
    pub const fn new(value: T) -> OptCell<T> {
        Self {
            data: UnsafeCell::new(Some(RefCell::new(value))),
        }
    }

    pub const fn none() -> OptCell<T> {
        Self {
            data: UnsafeCell::new(None),
        }
    }

    #[inline]
    unsafe fn get_unchecked(&self) -> &Option<RefCell<T>> {
        std::mem::transmute::<_, _>(self.data.get())
    }

    #[inline]
    unsafe fn get_mut_unchecked(&self) -> &'a mut Option<RefCell<T>> {
        std::mem::transmute::<_, _>(self.data.get())
    }

    #[inline]
    pub fn borrow(&self) -> Ref<'_, T> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => refcell.borrow(),
            _ => panic!("trying to borrow a None value"),
        }
    }

    #[inline]
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => refcell.borrow_mut(),
            _ => panic!("trying to borrow a None value"),
        }
    }

    #[inline]
    pub fn try_borrow(&self) -> Result<Ref<'_, T>, ()> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => refcell.try_borrow().map_err(|_| ()),
            _ => Err(()),
        }
    }

    #[inline]
    pub fn try_borrow_mut(&self) -> Result<RefMut<'_, T>, ()> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => refcell.try_borrow_mut().map_err(|_| ()),
            _ => Err(()),
        }
    }

    #[inline]
    pub fn take(&self) -> OptCell<T> {
        self.try_take().expect("already borrowed or moved")
    }

    pub fn replace(&self, data: T) -> Option<T> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => {
                if let Ok(mut borrowed) = refcell.try_borrow_mut() {
                    Some(std::mem::replace(&mut *borrowed, data))
                } else {
                    None
                }
            }
            None => {
                // SAFETY: since data is a None value, borrows are impossible
                // during this state
                unsafe { self.get_mut_unchecked().replace(RefCell::new(data)) };
                None
            }
        }
    }

    pub fn try_take(&self) -> Option<OptCell<T>> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => {
                // SAFETY: we pretend to borrow RefCell mutably,
                // ensuring that the data must not have any borrows
                if refcell.try_borrow_mut().is_err() {
                    return None;
                }
                // We must not use RefCell after this
                std::mem::drop(refcell);
                let value = unsafe { self.get_mut_unchecked().take() };
                Some(Self {
                    data: UnsafeCell::new(value),
                })
            }
            None => None,
        }
    }

    pub fn into_inner(self) -> Option<T> {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => {
                // SAFETY: we pretend to borrow RefCell mutably,
                // ensuring that the data must not have any borrows
                if refcell.try_borrow_mut().is_err() {
                    return None;
                }
                // We must not use RefCell after this
                std::mem::drop(refcell);
                let value = unsafe { self.get_mut_unchecked().take() };
                Some(value.unwrap().into_inner())
            }
            None => None,
        }
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        unsafe { self.get_unchecked() }.is_some()
    }

    #[inline]
    pub fn is_none(&self) -> bool {
        unsafe { self.get_unchecked() }.is_none()
    }
}

impl<T: Clone> Clone for OptCell<T> {
    #[inline]
    fn clone(&self) -> OptCell<T> {
        let data = unsafe { self.get_unchecked() };
        Self {
            data: UnsafeCell::new(data.clone()),
        }
    }
}

impl<T: Sized + fmt::Debug> fmt::Debug for OptCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match unsafe { self.get_unchecked() } {
            Some(refcell) => write!(f, "Some({:?})", refcell),
            None => write!(f, "None"),
        }
    }
}

#[cfg(test)]
mod optcell_test {
    use super::OptCell;

    #[test]
    fn drop_only_once() {
        use std::sync::atomic::{AtomicBool, Ordering};
        static FLAG: AtomicBool = AtomicBool::new(false);
        struct Empty {}
        impl Drop for Empty {
            fn drop(&mut self) {
                assert_eq!(FLAG.swap(true, Ordering::Relaxed), false);
            }
        }
        let cell = OptCell::new(5);
        cell.take();
    }

    #[test]
    fn move_after_moved() {
        let cell = OptCell::new(5);
        cell.take();
        assert!(cell.try_take().is_none());
    }

    #[test]
    fn borrow_after_moved() {
        let cell = OptCell::new(5);
        cell.take();
        assert!(cell.try_borrow().is_err());
    }

    #[test]
    fn move_after_borrowed() {
        let cell = OptCell::new(5);
        let borrowed = cell.borrow();
        assert!(cell.try_take().is_none());
        assert_eq!(*borrowed, 5);
    }

    #[test]
    fn replace_after_borrowed_mut() {
        let cell = OptCell::new(5);
        let borrowed = cell.borrow_mut();
        assert!(cell.replace(10).is_none());
        assert_eq!(*borrowed, 5);
    }

    #[test]
    fn replace_after_moved() {
        let cell = OptCell::new(5);
        cell.replace(10);
        assert_eq!(cell.try_take().unwrap().into_inner().unwrap(), 10);
    }

    #[test]
    fn borrow_after_replaced() {
        let cell = OptCell::new(5);
        cell.replace(10);
        assert_eq!(*cell.borrow(), 10);
    }

    #[test]
    fn borrow_after_move_and_replaced() {
        let cell = OptCell::new(5);
        cell.take();
        cell.replace(10);
        assert_eq!(*cell.borrow(), 10);
    }
}
