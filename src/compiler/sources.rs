use codespan_reporting::files::Files;
use std::collections::HashMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

pub type FileIndex = usize;
pub type SpanIndex = u32;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct SourceSpan {
    pub file: FileIndex,
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn from_tuple(tuple: (usize, usize)) -> Self {
        Self {
            file: 0,
            start: tuple.0,
            end: tuple.1,
        }
    }
}

pub struct Sources {
    sources_store: Vec<String>,
    sources_name: Vec<String>,
    sources_lines_to_byte_index: Vec<Vec<usize>>,
    sources_map: HashMap<PathBuf, FileIndex>,

    index_to_span: Vec<SourceSpan>,
    span_to_index: HashMap<SourceSpan, SpanIndex>,

    has_main_file: bool,
}

impl Sources {
    pub fn with_main_file<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let mut sources = Self {
            sources_store: vec![],
            sources_name: vec![],
            sources_lines_to_byte_index: vec![],
            sources_map: hashmap![],
            index_to_span: vec![],
            span_to_index: HashMap::new(),
            has_main_file: true,
        };
        if path.as_ref().is_relative() {
            let mut current_dir = std::env::current_dir()?;
            current_dir.push(path.as_ref());
            current_dir = std::fs::canonicalize(current_dir)?;
            sources.read(&current_dir)?;
        } else {
            sources.read(path)?;
        }
        Ok(sources)
    }

    pub fn new() -> Self {
        Self {
            sources_store: vec![],
            sources_name: vec![],
            sources_lines_to_byte_index: vec![],
            sources_map: hashmap![],
            index_to_span: vec![],
            span_to_index: HashMap::new(),
            has_main_file: false,
        }
    }

    pub fn main_file(&self) -> Option<&String> {
        if self.has_main_file {
            self.sources_store.first()
        } else {
            None
        }
    }

    pub fn read<P: AsRef<Path>>(&mut self, path: P) -> std::io::Result<(FileIndex, &String)> {
        assert!(path.as_ref().is_absolute());
        if let Some(source) = self.sources_map.get(path.as_ref()).cloned() {
            Ok((source, &self.sources_store[source]))
        } else {
            let vec = std::fs::read(path.as_ref())?;
            let len = self.sources_store.len();
            let path_buf = path.as_ref().to_path_buf();
            self.sources_name
                .push(path_buf.to_string_lossy().to_string());

            let source: String = String::from_utf8_lossy(vec.as_slice()).into();
            let mut line_contents = vec![];
            let mut row_start = 0;
            for (idx, ch) in source.chars().enumerate() {
                match ch {
                    '\n' => {
                        line_contents.push(row_start);
                        row_start = idx + 1;
                    }
                    _ => (),
                }
            }
            line_contents.push(row_start);
            self.sources_lines_to_byte_index.push(line_contents);
            self.sources_store.push(source);

            self.sources_map.insert(path_buf, len);
            Ok((len, self.sources_store.last().unwrap()))
        }
    }

    pub fn insert_span(&mut self, span: SourceSpan) -> SpanIndex {
        if let Some(idx) = self.span_to_index.get(&span).cloned() {
            return idx;
        }
        let len = self.index_to_span.len() as SpanIndex;
        self.index_to_span.push(span);
        self.span_to_index.insert(span, len);
        len
    }

    pub fn get_span(&self, idx: SpanIndex) -> Option<SourceSpan> {
        self.index_to_span.get(idx as usize).cloned()
    }
}

impl<'a> Files<'a> for Sources {
    type FileId = FileIndex;
    type Name = &'a String;
    type Source = &'a String;

    fn name(&'a self, id: Self::FileId) -> Option<Self::Name> {
        self.sources_name.get(id)
    }

    fn source(&'a self, id: Self::FileId) -> Option<Self::Source> {
        self.sources_store.get(id)
    }

    fn line_index(&'a self, id: Self::FileId, byte_index: usize) -> Option<usize> {
        if let Some(source) = self.source(id) {
            if byte_index > source.len() {
                return None;
            }
            let x = match self.sources_lines_to_byte_index[id]
                .as_slice()
                .binary_search(&byte_index)
            {
                Ok(x) => x,
                Err(x) => x - 1,
            };
            // dbg_println!("line_idx {} => {:?}", byte_index, x);
            Some(x)
        } else {
            None
        }
    }

    fn line_range(&'a self, id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
        if let Some(source) = self.source(id) {
            let line_info = &self.sources_lines_to_byte_index[id];
            // dbg_println!("line_range {} => {:?}..{:?}", line_index, line_info.get(line_index), line_info.get(line_index + 1));
            match (line_info.get(line_index), line_info.get(line_index + 1)) {
                (Some(x), Some(y)) => Some((*x)..(*y)),
                (Some(x), None) => Some((*x)..source.len()),
                (None, Some(_)) => None,
                (None, None) => None,
            }
        } else {
            None
        }
    }
}
