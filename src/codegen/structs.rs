use crate::utils::optcell::OptCell;
use std::cell::Ref;
use std::cmp::max;
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

    pub fn int_from_bits(bits: u16) -> Option<Self> {
        match bits {
            8 => Some(StructFieldType::I8),
            16 => Some(StructFieldType::I16),
            32 => Some(StructFieldType::I32),
            64 => Some(StructFieldType::I64),
            _ => None,
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
    flattened_fields: OptCell<Vec<StructField>>,
    size_of: u32,
    align_of: u32,
}

impl StructData {
    pub fn new() -> Self {
        Self {
            fields: vec![],
            flattened_fields: OptCell::none(),
            size_of: 0,
            align_of: 1,
        }
    }

    pub fn append_typed(&mut self, typed: StructFieldType) {
        self.append_array(typed, 1);
    }

    pub fn append_struct(&mut self, data: Rc<StructData>) {
        self.append_struct_array(data, 1);
    }

    pub fn append_array(&mut self, typed: StructFieldType, len: u32) {
        let size_of = typed.size_of();
        self.align_of = max(self.align_of, size_of);
        self.size_of = align(self.size_of, size_of);
        self.fields.push(StructField {
            offset: self.size_of,
            multiplier: len,
            typed,
        });
        self.size_of += size_of * len;
    }

    pub fn append_struct_array(&mut self, data: Rc<StructData>, len: u32) {
        let size_of = data.size_of();
        self.align_of = max(self.align_of, size_of);
        self.size_of = align(self.size_of, data.align_of());
        self.fields.push(StructField {
            offset: self.size_of,
            multiplier: len,
            typed: StructFieldType::Struct(data),
        });
        self.size_of += size_of * len;
    }

    pub fn fields(&self) -> &Vec<StructField> {
        &self.fields
    }

    pub fn flattened_fields(&self) -> Ref<'_, Vec<StructField>> {
        if let Ok(Some(flattened_fields)) = self.flattened_fields.try_borrow() {
            flattened_fields
        } else {
            let mut vec = vec![];
            for field in &self.fields {
                match &field.typed {
                    StructFieldType::Struct(x) => vec.extend_from_slice(&x.flattened_fields()),
                    _other => vec.push(field.clone()),
                }
            }
            self.flattened_fields.replace(vec);
            self.flattened_fields.borrow().unwrap()
        }
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
            bytes: vec![0; data.size_of() as usize],
        }
    }

    pub fn append_zeroed(&mut self) {
        let field = &self.data.fields()[self.field_cursor];
        for i in (field.offset)..(field.offset + field.typed.size_of()) {
            self.bytes[i as usize] = 0;
        }
        self.field_cursor += 1;
    }

    pub fn append(&mut self, bytes: &[u8]) {
        let field = &self.data.fields()[self.field_cursor];
        assert_eq!(field.typed.size_of(), bytes.len() as u32);
        for (i, byte) in ((field.offset)..(field.offset + field.typed.size_of())).zip(bytes.iter())
        {
            self.bytes[i as usize] = *byte;
        }
        self.field_cursor += 1;
    }

    pub fn into_vec(self) -> Vec<u8> {
        self.bytes
    }
}

pub struct UnionBuilder {
    fields: Vec<StructField>,
    size_of: u32,
    align_of: u32,
}

impl UnionBuilder {
    pub fn new() -> Self {
        Self {
            fields: vec![],
            size_of: 0,
            align_of: 1,
        }
    }

    pub fn into_struct_data(mut self) -> StructData {
        // FIXME: we should determine the optimal integer type
        // for our discriminant field, but i32 will suffice for now
        let discriminant_typed = StructFieldType::I32;
        let start_offset = max(self.align_of, discriminant_typed.size_of());
        for field in &mut self.fields {
            field.offset = start_offset;
        }
        self.fields.insert(
            0,
            StructField {
                offset: 0,
                multiplier: 1,
                typed: discriminant_typed,
            },
        );
        StructData {
            fields: self.fields,
            flattened_fields: OptCell::none(),
            size_of: self.size_of + start_offset,
            align_of: self.align_of,
        }
    }

    pub fn insert_typed(&mut self, typed: StructFieldType) {
        let size_of = typed.size_of();
        self.align_of = max(self.align_of, size_of);
        self.size_of = max(self.size_of, size_of);
        self.fields.push(StructField {
            offset: 0,
            multiplier: 1,
            typed,
        });
    }

    pub fn insert_struct(&mut self, data: Rc<StructData>) {
        self.align_of = max(self.align_of, data.align_of());
        self.size_of = max(self.size_of, data.size_of());
        self.fields.push(StructField {
            offset: 0,
            multiplier: 1,
            typed: StructFieldType::Struct(data),
        });
    }
}
