use std::collections::{hash_map::Entry, HashMap, HashSet, VecDeque};

use itertools::Itertools;

#[derive(Debug)]
pub struct ScoredSearch<T, S> {
    queue: VecDeque<T>,
    seen: HashMap<T, S>,
    dfs: bool,
    discard_count: u64,
    insert_count: u64,
}

impl<T: std::fmt::Debug + PartialEq + Eq + std::hash::Hash + Clone, S: Ord + Copy>
    ScoredSearch<T, S>
{
    pub fn new_dfs() -> ScoredSearch<T, S> {
        ScoredSearch {
            queue: VecDeque::new(),
            seen: HashMap::new(),
            dfs: true,
            discard_count: 0,
            insert_count: 0,
        }
    }

    pub fn push(&mut self, entry: T, score: S) -> bool {
        let score_entry = self.seen.entry(entry.clone());

        match score_entry {
            Entry::Occupied(mut o) => {
                // TODO: let min/max be configurable
                if o.get() < &score {
                    // we've already hit this state with a better score, so abort this search
                    self.discard_count += 1;
                    return false;
                }
                o.insert(score);
            }
            Entry::Vacant(v) => {
                // we haven't seen this state before, so this score is the best so far
                v.insert(score);
            }
        }
        self.insert_count += 1;

        if self.dfs {
            self.queue.push_front(entry);
        } else {
            self.queue.push_back(entry);
        }
        true
    }

    pub fn pop(&mut self) -> Option<(T, S)> {
        // todo: this clone should be avoidable
        self.queue.pop_front().map(|x| (x.clone(), self.seen[&x]))
    }

    pub fn get_scores_matching(&self, predicate: impl Fn(&T) -> bool) -> Vec<S> {
        self.seen
            .iter()
            .filter(|(t, _s)| predicate(*t))
            .map(|(_t, s)| s)
            .copied()
            .collect_vec()
    }

    pub fn debug_info(&self) -> String {
        format!(
            "dsc: {} ins: {} len: {} seen: {}",
            self.discard_count,
            self.insert_count,
            self.queue.len(),
            self.seen.len()
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
