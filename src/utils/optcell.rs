use std::cell::{BorrowError, BorrowMutError, Cell, Ref, RefCell, RefMut};
use std::mem::ManuallyDrop;

#[derive(Debug)]
pub struct OptCell<T> {
    data: ManuallyDrop<RefCell<T>>,
    moved: Cell<bool>,
}

impl<T> OptCell<T> {
    pub const fn new(value: T) -> OptCell<T> {
        Self {
            data: ManuallyDrop::new(RefCell::new(value)),
            moved: Cell::new(false),
        }
    }

    #[inline]
    pub fn borrow(&self) -> Ref<'_, T> {
        self.data.borrow()
    }

    #[inline]
    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.data.borrow_mut()
    }

    #[inline]
    pub fn try_borrow(&self) -> Result<Ref<'_, T>, BorrowError> {
        self.data.try_borrow()
    }

    #[inline]
    pub fn try_borrow_mut(&self) -> Result<RefMut<'_, T>, BorrowMutError> {
        self.data.try_borrow_mut()
    }

    pub fn take(&self) -> OptCell<T> {
        self.try_take().expect("already moved")
    }

    pub fn try_take(&self) -> Option<OptCell<T>> {
        // SAFETY: we borrow the RefCell mutably, and never return it back.
        // Doing so will prevent future borrows from occuring.
        // We also set the moved flag to prevent double frees
        if self.moved.replace(true) {
            return None;
        }
        if let Ok(mut borrowed) = self.data.try_borrow_mut() {
            let data = unsafe { std::mem::replace(&mut *borrowed, std::mem::zeroed()) };
            std::mem::forget(borrowed);
            Some(Self::new(data))
        } else {
            None
        }
    }

    pub fn into_inner(mut self) -> T {
        if self.moved.replace(true) {
            panic!("already moved");
        }
        unsafe { ManuallyDrop::take(&mut self.data) }.into_inner()
    }
}

impl<T> Drop for OptCell<T> {
    fn drop(&mut self) {
        // SAFETY: We should only drop the data if we own it, when it isn't moved
        if !self.moved.get() {
            unsafe { ManuallyDrop::drop(&mut self.data) }
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
    fn move_after_borrowed_mut() {
        let cell = OptCell::new(5);
        let borrowed = cell.borrow_mut();
        assert!(cell.try_take().is_none());
        assert_eq!(*borrowed, 5);
    }
}
