use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash};

pub struct OverlayHashMap<'a, K, V, S> {
    first: &'a mut HashMap<K, V, S>,
    second: Option<&'a HashMap<K, V, S>>,
}

impl<'a, K, V, S> OverlayHashMap<'a, K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    pub fn new(first: &'a mut HashMap<K, V, S>, second: Option<&'a HashMap<K, V, S>>) -> Self {
        OverlayHashMap { first, second }
    }

    pub fn map_mut(&mut self) -> &mut HashMap<K, V, S> {
        self.first
    }

    pub fn map_imm(&self) -> Option<&HashMap<K, V, S>> {
        self.second
    }

    pub fn contains_key<Q: Sized>(&self, key: Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.first.contains_key(&key)
            || self
                .second
                .map(|map| map.contains_key(&key))
                .unwrap_or(false)
    }

    pub fn get<Q: Sized>(&self, key: Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.first
            .get(&key)
            .or_else(|| self.second.map(|map| map.get(&key)).flatten())
    }
}

impl<'a, K, V: Clone, S> OverlayHashMap<'a, K, V, S>
where
    K: Hash + Eq,
    S: BuildHasher,
{
    pub fn get_or_clone<Q: Sized>(&mut self, key: Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + Into<K>,
    {
        if self.first.contains_key(&key) {
            self.first.get_mut(&key)
        } else if let Some(v) = self.second.map(|map| map.get(&key)).flatten() {
            Some(self.first.entry(key.into()).or_insert_with(|| v.clone()))
        } else {
            None
        }
    }
}

macro_rules! overlay_hashmap {
    ($first:expr, $second:expr) => {
        OverlayHashMap::new($first, $second)
    };
}
