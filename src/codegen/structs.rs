use crate::ssa::isa::Type;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct StructFieldData {
    pub offset: usize,
    pub typed: Type,
}

#[derive(Debug, Clone)]
pub struct StructData {
    values: HashMap<Rc<str>, StructFieldData>,
    size_of: usize,
}

impl StructData {
    pub fn new() -> Self {
        StructData {
            values: HashMap::new(),
            size_of: 0,
        }
    }

    pub fn values(&self) -> &HashMap<Rc<str>, StructFieldData> {
        &self.values
    }

    pub fn size_of(&self) -> usize {
        self.size_of
    }

    pub fn append_type(&mut self, name: Rc<str>, typed: Type) {
        let type_size = typed.bytes().unwrap();
        self.values.insert(
            name,
            StructFieldData {
                offset: self.size_of,
                typed,
            },
        );
        self.size_of += type_size;
    }
}

/// Struct builder
pub struct StructBuilder<'a> {
    vec: Vec<u8>,
    data: &'a StructData,
}

impl<'a> StructBuilder<'a> {
    pub fn new(data: &'a StructData) -> Self {
        StructBuilder {
            vec: vec![0; data.size_of()],
            data,
        }
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.vec
    }

    pub fn insert(&mut self, name: &str, bytes: &[u8]) -> Option<usize> {
        if let Some(data) = self.data.values().get(name) {
            if data.typed.bytes().unwrap() != bytes.len() {
                return None;
            }
            for (i, &byte) in bytes.iter().enumerate() {
                self.vec[data.offset + i] = byte;
            }
            return Some(data.offset);
        }
        None
    }

    pub fn insert_zeroed(&mut self, name: &str) -> Option<usize> {
        if let Some(data) = self.data.values().get(name) {
            for i in data.offset..(data.offset + data.typed.bytes().unwrap()) {
                self.vec[i] = 0;
            }
            return Some(data.offset);
        }
        None
    }
}
