use std::collections::VecDeque;

#[derive(Debug)]
pub struct ScoredSearch<T> {
    queue: VecDeque<T>,
    dfs: bool,
    discard_count: u64,
    insert_count: u64,
}

impl<T: std::fmt::Debug + PartialEq + Eq + std::hash::Hash + Clone + PartialOrd> ScoredSearch<T> {
    pub fn new_dfs() -> ScoredSearch<T> {
        ScoredSearch {
            queue: VecDeque::new(),
            dfs: true,
            discard_count: 0,
            insert_count: 0,
        }
    }

    pub fn push(&mut self, entry: T) -> bool {
        /*
        if !self.queue.is_empty()) &&  self.queue.iter().all(|x| x <= &entry) {
            self.discard_count += 1;
            return false;
        }
        self.insert_count += 1;
        */

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

    pub fn debug_info(&self) -> String {
        format!(
            "dsc: {} ins: {} len: {}",
            self.discard_count,
            self.insert_count,
            self.queue.len()
        )
    }
}

/*
pub trait ScoredSearchExt<T> {
    fn push_opt(&mut self, entry: Option<T>) -> bool;
}

impl<T: std::fmt::Debug + PartialEq + Eq + std::hash::Hash + Clone, S:Ord> ScoredSearchExt<T, S>
    for ScoredSearch<T, S>
{
    fn push_opt(&mut self, entry: Option<T>, ) -> bool {
        if let Some(entry) = entry {
            self.push(entry);
            return true;
        }
        false
    }
}
*/
