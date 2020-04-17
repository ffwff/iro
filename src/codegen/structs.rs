use crate::ssa::isa::Type;
use std::rc::Rc;

fn align(value: u32, to: u32) -> u32 {
    (value + to - 1) & !(to - 1)
}

#[derive(Debug, Clone)]
pub enum StructFieldType {
    I8,
    I16,
    I32,
    I64,
    F64,
    Struct(Rc<StructData>),
}

impl StructFieldType {
    pub fn size_of(&self) -> u32 {
        match self {
            StructFieldType::I8 => 1,
            StructFieldType::I16 => 2,
            StructFieldType::I32 => 4,
            StructFieldType::I64 => 8,
            StructFieldType::F64 => 8,
            StructFieldType::Struct(n) => n.size_of(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub offset: u32,
    pub multiplier: u32,
    pub typed: StructFieldType,
}

#[derive(Debug, Clone)]
pub struct StructData {
    fields: Vec<StructField>,
    size_of: u32,
    align_of: u32,
}

impl StructData {
    pub fn new() -> Self {
        Self {
            fields: vec![],
            size_of: 0,
            align_of: 0,
        }
    }

    pub fn append_typed(&mut self, typed: StructFieldType) {
        assert!(typed.is_primitive());
        self.size_of = align(self.size_of, typed.size_of());
        self.fields.push(StructField {
            offset: self.size_of,
            multiplier: 1,
            typed,
        });
        self.size_of += typed.size_of();
    }

    pub fn append_struct(&mut self, data: Rc<StructData>) {
        self.size_of = align(self.size_of, data.align_of());
        self.fields.push(StructField {
            offset: self.size_of,
            multiplier: 1,
            typed: StructFieldType::Struct(data),
        });
        self.size_of += data.size_of();
    }

    pub fn append_array(&mut self, typed: StructFieldType, len: u32) {
        self.size_of = align(self.size_of, typed.size_of());
        self.fields.push(StructField {
            offset: self.size_of,
            multiplier: len,
            typed,
        });
        self.size_of += typed.size_of() * len;
    }

    pub fn fields(&self) -> &Vec<StructField> {
        &self.fields
    }

    pub fn fields_flatten(&self) -> Vec<StructFieldType> {
        let mut vec = vec![];
        for field in &self.fields {
            for _ in 0..field.multiplier {
                match field.typed {
                    Type::Struct(x) => vec.extend_from_slice(&x.fields_flatten()),
                    other => vec.push(other),
                }
            }
        }
        vec
    }

    pub fn size_of(&self) -> u32 {
        self.size_of
    }

    pub fn align_of(&self) -> u32 {
        self.align_of
    }
}

pub struct StructBuilder<'a> {
    data: &'a StructData,
    field_cursor: usize,
    bytes: Vec<u8>,
}

impl<'a> StructBuilder<'a> {
    pub fn new(data: &'a StructData) -> Self {
        Self {
            data,
            field_cursor: 0,
            bytes: vec![0; data.size_of()],
        }
    }

    pub fn append_zeroed(&mut self) {
        let field = &self.data.fields()[self.field_cursor];
        for i in (field.offset)..(field.offset + field.size_of) {
            self.bytes[i as usize] = 0;
        }
        self.field_cursor += 1;
    }

    pub fn append(&mut self, bytes: &[u8]) {
        let field = &self.data.fields()[self.field_cursor];
        assert!(field.size_of == bytes.len());
        for (i, byte) in ((field.offset)..(field.offset + field.size_of)).zip(bytes.iter()) {
            self.bytes[i as usize] = byte;
        }
        self.field_cursor += 1;
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.bytes
    }
}