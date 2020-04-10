#[repr(C)]
pub struct FatPointer<T> {
    data: *mut T,
    len: usize,
}

impl<T: Sized> FatPointer<T> {
    pub fn new(data: *mut T, len: usize) -> Self {
        Self { data, len }
    }

    pub fn data(&self) -> *mut T {
        self.data
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub unsafe fn slice<'a>(&self) -> &'a [T] {
        std::slice::from_raw_parts(self.data as *const T, self.len)
    }

    pub unsafe fn slice_mut<'a>(&self) -> &'a mut [T] {
        std::slice::from_raw_parts_mut(self.data as *mut T, self.len)
    }
}
