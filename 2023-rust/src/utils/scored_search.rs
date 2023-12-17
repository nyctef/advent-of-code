use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

/// T: the state type
/// K: a unique key for the state for looking up scores
/// S: the type of the score
///
/// here we only require PartialOrd for the score, which mean a given
/// state might have several "best" scores. This is suboptimal if
/// the score is actually Ord, but makes the algorithm more flexible
///
/// by default the score is minimized
pub struct ScoredSearch<T, K, S> {
    queue: VecDeque<T>,
    best_scores: HashMap<K, Vec<S>>,
    // TODO: might be able to remove need for Box<...> here if we just
    // make this one big function rather than storing these funcs in a struct
    get_key: Box<dyn Fn(&T) -> K>,
    get_score: Box<dyn Fn(&T) -> S>,
    dfs: bool,
    discard_count: u64,
    insert_count: u64,
}

#[allow(dead_code)]
impl<T: std::fmt::Debug + Clone, K: Eq + PartialEq + Hash, S: Clone + Copy + PartialOrd>
    ScoredSearch<T, K, S>
{
    pub fn new_dfs(
        get_key: impl Fn(&T) -> K + 'static,
        get_score: impl Fn(&T) -> S + 'static,
    ) -> ScoredSearch<T, K, S> {
        ScoredSearch {
            queue: VecDeque::new(),
            best_scores: HashMap::new(),
            get_key: Box::new(get_key),
            get_score: Box::new(get_score),
            dfs: true,
            discard_count: 0,
            insert_count: 0,
        }
    }

    pub fn new_bfs(
        get_key: impl Fn(&T) -> K + 'static,
        get_score: impl Fn(&T) -> S + 'static,
    ) -> ScoredSearch<T, K, S> {
        ScoredSearch {
            queue: VecDeque::new(),
            best_scores: HashMap::new(),
            get_key: Box::new(get_key),
            get_score: Box::new(get_score),
            dfs: false,
            discard_count: 0,
            insert_count: 0,
        }
    }

    pub fn push(&mut self, entry: T) -> bool {
        let key = (self.get_key)(&entry);
        let score = (self.get_score)(&entry);
        let bests = self.best_scores.entry(key).or_insert(vec![]);
        if bests.iter().any(|b| b <= &score) {
            // we've already reached this position with an equal or better score, so skip
            self.discard_count += 1;
            return false;
        } else {
            self.insert_count += 1;
            bests.push(score);
            // only retain scores that aren't strictly worse than the one we've just added
            // this makes future score checks faster since we have to check against fewer items
            bests.retain(|b| !(b > &score));
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

    pub fn debug_info(&self) -> String {
        format!(
            "dsc: {} ins: {} len: {}",
            self.discard_count,
            self.insert_count,
            self.queue.len()
        )
    }

    pub fn get_best_scores(&self) -> &HashMap<K, Vec<S>> {
        // TODO: move more logic for reconstructing the path into here
        // probably easiest if we build up a lookup of "previous" nodes for each state
        &self.best_scores
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
