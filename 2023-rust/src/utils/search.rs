use std::collections::{HashSet, VecDeque};

#[derive(Debug)]
pub struct Search<T> {
    queue: VecDeque<T>,
    seen: HashSet<T>,
    dfs: bool,
}

impl<T: std::fmt::Debug + PartialEq + Eq + std::hash::Hash + Clone> Search<T> {
    pub fn new_dfs() -> Search<T> {
        Search {
            queue: VecDeque::new(),
            seen: HashSet::new(),
            dfs: true,
        }
    }

    pub fn push(&mut self, entry: T) -> bool {
        if self.seen.contains(&entry) {
            return false;
        }
        // todo: some workaround like https://github.com/rust-lang/rust/issues/60896 to avoid the
        // double lookup here?
        self.seen.insert(entry.clone());
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
