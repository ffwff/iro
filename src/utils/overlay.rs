use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

pub struct OverlayHashMap<'a, K, V> {
    first: &'a mut HashMap<K, V>,
    second: Option<&'a HashMap<K, V>>,
}

impl<'a, K, V> OverlayHashMap<'a, K, V>
where
    K: Hash + Eq,
{
    pub fn new(first: &'a mut HashMap<K, V>, second: Option<&'a HashMap<K, V>>) -> Self {
        OverlayHashMap { first, second }
    }

    pub fn map_mut(&mut self) -> &mut HashMap<K, V> {
        self.first
    }

    pub fn map_imm(&self) -> Option<&HashMap<K, V>> {
        self.second.clone()
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
}

impl<'a, K, V: Clone> OverlayHashMap<'a, K, V>
where
    K: Hash + Eq,
{
    pub fn get_or_clone<Q: Sized>(&mut self, key: Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + Into<K>,
    {
        if self.first.contains_key(&key) {
            self.first.get_mut(&key)
        } else if let Some(v) = self.second.map(|map| map.get(&key)).flatten() {
            Some(self.first.entry(key.into()).or_insert(v.clone()))
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
