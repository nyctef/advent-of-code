use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

use color_eyre::Section;

/// T: the state type
/// K: a unique key for the state for looking up scores
/// S: the type of the score
///
/// here we only require PartialOrd for the score, which mean a given
/// state might have several "best" scores. This is suboptimal if
/// the score is actually Ord, but makes the algorithm more flexible
///
/// by default the score is minimized (TODO: rename score to cost to make this clear)
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
impl<
        T: std::fmt::Debug + Clone,
        K: std::fmt::Debug + Eq + PartialEq + Hash,
        S: std::fmt::Debug + Clone + Copy + PartialOrd,
    > ScoredSearch<T, K, S>
{
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

    pub fn run(
        &mut self,
        get_next_candidates: impl Fn(T) -> Vec<T>,
        is_target_state: impl Fn(&T) -> bool,
        max_score: S,
    ) -> (Vec<S>, Option<T>) {
        let mut bests = vec![max_score];
        let mut best_state = None;
        let mut count: u64 = 0;
        while let Some(current_state) = self.pop() {
            if is_target_state(&current_state) {
                let score = (self.get_score)(&current_state);
                if !bests.iter().any(|b| b <= &score) {
                    // println!("found a potential best {:?} with score {:?}", &current_state, score);
                    bests.push(score);
                    best_state = Some(current_state.clone());
                    bests.retain(|b| b.partial_cmp(&score) != Some(Ordering::Greater));
                }
            }
            if count % 1_000_000 == 0 {
                println!("{} best: {:?}", self.debug_info(), bests);
            }
            count += 1;
            let candidates = get_next_candidates(current_state);
            for c in candidates {
                /*
                let min_cost_to_end =
                    c.loss as usize + RCDirection::from_to(&c.pos, &target).manhattan_abs();
                if min_cost_to_end > best as usize {
                    // assuming every tile between here and the target was 1, we still wouldn't
                    // be able to beat the current best score
                    continue;
                }
                */

                self.push(c);
            }
        }
        println!("final bests: {:?}", bests);
        // dbg!(&self.best_scores);
        (bests, best_state)
    }

    pub fn push(&mut self, entry: T) -> bool {
        let key = (self.get_key)(&entry);
        let score = (self.get_score)(&entry);
        let bests = self.best_scores.entry(key).or_default();
        if bests.iter().any(|b| b <= &score) {
            // we've already reached this position with an equal or better score, so skip
            self.discard_count += 1;
            return false;
        } else {
            self.insert_count += 1;
            bests.push(score);
            // only retain scores that aren't strictly worse than the one we've just added.
            // this makes future score checks faster since we have to check against fewer items.
            // detail: we keep `b` if `b` is Less, Equal or None (incomparable) compared to `score`
            bests.retain(|b| b.partial_cmp(&score) != Some(Ordering::Greater));
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
