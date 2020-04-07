use crate::ssa::isa::Type;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct StructFieldData {
    pub offset: usize,
    pub typed: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    I8,
    I16,
    I32,
    I64,
    F64,
}

impl PrimitiveType {
    pub fn from_type(typed: &Type) -> Self {
        match typed {
            Type::I8 => PrimitiveType::I8,
            Type::I16 => PrimitiveType::I16,
            Type::I32 | Type::I32Ptr(_) => PrimitiveType::I32,
            Type::I64 | Type::I64Ptr(_) => PrimitiveType::I64,
            Type::F64 => PrimitiveType::F64,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrimitiveTypeField {
    pub offset: usize,
    pub typed: PrimitiveType,
}

#[derive(Debug, Clone)]
pub struct StructData {
    values: HashMap<Rc<str>, StructFieldData>,
    flattened: Vec<PrimitiveTypeField>,
    size_of: usize,
    name: Rc<str>,
}

impl StructData {
    pub fn new(name: Rc<str>) -> Self {
        StructData {
            values: HashMap::new(),
            flattened: vec![],
            size_of: 0,
            name,
        }
    }

    pub fn name(&self) -> &Rc<str> {
        &self.name
    }

    pub fn values(&self) -> &HashMap<Rc<str>, StructFieldData> {
        &self.values
    }

    pub fn flattened(&self) -> &Vec<PrimitiveTypeField> {
        &self.flattened
    }

    pub fn size_of(&self) -> usize {
        self.size_of
    }

    pub fn append_type(&mut self, name: Rc<str>, typed: Type) {
        fn align(value: usize, to: usize) -> usize {
            (value + to - 1) & !(to - 1)
        }
        let type_size = typed.bytes().unwrap();
        self.size_of = align(self.size_of, type_size);
        self.flattened.push(PrimitiveTypeField {
            offset: self.size_of,
            typed: PrimitiveType::from_type(&typed),
        });
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

impl std::fmt::Display for StructData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
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
