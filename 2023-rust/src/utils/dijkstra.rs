use std::cmp::Reverse;
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap};
use std::hash::Hash;

/// T: the state type
/// K: a unique key for the state for looking up scores
///
pub struct Dijkstra<T, K> {
    queue: BinaryHeap<Reverse<T>>,
    // TODO: might be able to remove need for Box<...> here if we just
    // make this one big function rather than storing these funcs in a struct
    get_key: Box<dyn Fn(&T) -> K>,
    bests: HashMap<K, T>,
}

#[allow(dead_code)]
impl<T: std::fmt::Debug + Clone + Ord, K: Eq + PartialEq + Hash> Dijkstra<T, K> {
    pub fn new(get_key: impl Fn(&T) -> K + 'static) -> Dijkstra<T, K> {
        Dijkstra {
            queue: BinaryHeap::new(),
            get_key: Box::new(get_key),
            bests: HashMap::new(),
        }
    }

    pub fn run(
        &mut self,
        get_next_candidates: impl Fn(T) -> Vec<T>,
        is_target_state: impl Fn(&T) -> bool,
    ) -> T {
        let mut count: u64 = 0;
        while let Some(Reverse(current_state)) = self.queue.pop() {
            if count % 1_000_000 == 0 {
                println!(
                    "c: {} ql: {} s: {:?} bl: {}",
                    count,
                    self.queue.len(),
                    current_state,
                    self.bests.len()
                );
            }
            count += 1;
            if is_target_state(&current_state) {
                // dijkstra relies on the assumption that if we always took
                // the smallest step from the lowest-cost node, then if
                // we reach the destination then we must have taken
                // the shortest path.
                return current_state;
            }
            let candidates = get_next_candidates(current_state);
            for c in candidates {
                let e = self.bests.entry((self.get_key)(&c));
                match e {
                    Entry::Occupied(mut o) => {
                        if &c < o.get() {
                            // we've seen this state before, but now
                            // we have a shorter path to it
                            o.insert(c.clone());
                        } else {
                            // we have a better or equal path to this
                            // state, so skip this candidate
                            continue;
                        }
                    }
                    Entry::Vacant(v) => {
                        // we haven't seen this state before, so
                        // we record the state as-is
                        v.insert(c.clone());
                    }
                }
                self.queue.push(Reverse(c));
            }
        }
        panic!("ran out of states before reaching the end");
    }

    pub fn push(&mut self, entry: T) {
        self.queue.push(Reverse(entry));
    }
    /*

    pub fn get_best_scores(&self) -> &HashMap<K, Vec<S>> {
        // TODO: move more logic for reconstructing the path into here
        // probably easiest if we build up a lookup of "previous" nodes for each state
        &self.best_scores
    }
    */
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
