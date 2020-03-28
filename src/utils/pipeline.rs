#[derive(Debug, Clone, Copy)]
pub enum Flow {
    Continue,
    Break,
}

#[derive(Debug, Clone)]
pub struct Pipeline<T: 'static, F> {
    funcs: Vec<F>,
    phantom: std::marker::PhantomData<&'static T>,
}

impl<T, F> Pipeline<T, F>
where
    T: Sized,
    F: Fn(&mut T) -> Flow,
{
    pub fn new(funcs: Vec<F>) -> Self {
        Self {
            funcs,
            phantom: std::marker::PhantomData,
        }
    }

    pub fn apply(&self, data: &mut T) {
        for func in &self.funcs {
            match func(data) {
                Flow::Continue => (),
                Flow::Break => return,
            }
        }
    }
}
