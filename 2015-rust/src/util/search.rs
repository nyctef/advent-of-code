use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

pub struct Search<T, K> {
    queue: VecDeque<T>,
    seen: HashSet<K>,
    get_key: Box<dyn Fn(&T) -> K>,
    dfs: bool,
    track_seen: bool,
}

#[allow(dead_code)]
impl<T: std::fmt::Debug + Clone, K: PartialEq + Eq + Hash> Search<T, K> {
    pub fn new_dfs(get_key: impl Fn(&T) -> K + 'static) -> Search<T, K> {
        Search {
            queue: VecDeque::new(),
            seen: HashSet::new(),
            get_key: Box::new(get_key),
            dfs: true,
            track_seen: true,
        }
    }

    pub fn new_bfs(get_key: impl Fn(&T) -> K + 'static) -> Search<T, K> {
        Search {
            queue: VecDeque::new(),
            seen: HashSet::new(),
            get_key: Box::new(get_key),
            dfs: false,
            track_seen: true,
        }
    }
    pub fn new_exhaustive(get_key: impl Fn(&T) -> K + 'static) -> Search<T, K> {
        Search {
            queue: VecDeque::new(),
            seen: HashSet::new(),
            get_key: Box::new(get_key),
            dfs: false,
            track_seen: false,
        }
    }

    pub fn push(&mut self, entry: T) -> bool {
        if self.track_seen {
            if self.seen.contains(&(self.get_key)(&entry)) {
                return false;
            }
            // todo: some workaround like https://github.com/rust-lang/rust/issues/60896 to avoid the
            // double lookup here?
            self.seen.insert((self.get_key)(&entry));
        }
        if self.dfs {
            self.queue.push_front(entry);
        } else {
            self.queue.push_back(entry);
        }
        true
    }

    pub fn pop(&mut self) -> Option<T> {
        self.queue.pop_front()
    }
}
