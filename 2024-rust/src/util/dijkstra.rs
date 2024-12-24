use std::cmp::{Ordering, Reverse};
use std::collections::hash_map::Entry;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::hash::Hash;

use itertools::Itertools;

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
impl<T: std::fmt::Debug + Clone + Ord, K: std::fmt::Debug + Eq + PartialEq + Hash> Dijkstra<T, K> {
    pub fn new(get_key: impl Fn(&T) -> K + 'static) -> Dijkstra<T, K> {
        Dijkstra {
            queue: BinaryHeap::new(),
            get_key: Box::new(get_key),
            bests: HashMap::new(),
        }
    }

    pub fn run_single(
        &mut self,
        get_next_candidates: impl FnMut(T) -> Vec<T>,
        is_target_state: impl Fn(&T) -> bool,
    ) -> Option<T> {
        self.run(get_next_candidates, &is_target_state, true);

        // we may have queued up several candidates for the final state
        // before processing one and quitting the loop, so now we find the smallest here:
        self.bests
            .values()
            .filter(|b| is_target_state(b))
            .min()
            .cloned()
    }

    pub fn run_all(
        &mut self,
        get_next_candidates: impl FnMut(T) -> Vec<T>,
        is_target_state: impl Fn(&T) -> bool,
    ) -> Vec<T> {
        self.run(get_next_candidates, &is_target_state, true);

        let result = self
            .bests
            .values()
            .filter(|b| is_target_state(b))
            .cloned()
            .collect_vec();

        if result.len() > 1 {
            assert!(result.iter().all(|s| s.cmp(&result[0]) == Ordering::Equal));
        }
        result
    }

    fn run(
        &mut self,
        mut get_next_candidates: impl FnMut(T) -> Vec<T>,
        is_target_state: impl Fn(&T) -> bool,
        stop_early: bool,
    ) {
        let mut count: u64 = 0;
        while let Some(Reverse(current_state)) = self.queue.pop() {
            if count % 100_000 == (100_000 - 1) {
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
                if stop_early {
                    return;
                } else {
                    continue;
                }
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
    }

    pub fn push(&mut self, entry: T) {
        self.queue.push(Reverse(entry.clone()));
        self.bests.insert((self.get_key)(&entry), entry);
    }

    pub fn reconstruct_path_single(
        &self,
        result: T,
        mut get_prev_candidates: impl FnMut(T) -> Vec<T>,
        is_starting_state: impl Fn(&T) -> bool,
    ) -> Vec<T> {
        let mut path = vec![];

        let mut next = result;
        'outer: loop {
            path.push(next.clone());

            if is_starting_state(&next) {
                break;
            }

            let next_candidates = get_prev_candidates(next.clone());

            for nc in next_candidates {
                let bests = self.bests.get(&(self.get_key)(&nc));

                if let Some(b) = bests {
                    if b.cmp(&nc) == Ordering::Equal {
                        // this looks like the right path
                        // we just take this one greedily since we don't want to
                        // handle there being multiple paths in this case
                        next = b.clone();
                        continue 'outer;
                    }
                }
            }
        }

        path.reverse();
        path
    }

    pub fn reconstruct_path_all(
        &self,
        result: T,
        mut get_prev_candidates: impl FnMut(&T) -> Vec<T>,
        is_starting_state: impl Fn(&T) -> bool,
    ) -> Vec<Vec<T>> {
        let mut paths = vec![];

        let mut q = VecDeque::new();
        q.push_front(vec![result]);
        while let Some(mut next) = q.pop_front() {
            let tail = next.last().unwrap();
            if is_starting_state(tail) {
                next.reverse();
                paths.push(next);
                continue;
            }

            let next_candidates = get_prev_candidates(tail);

            for nc in next_candidates {
                let bests = self.bests.get(&(self.get_key)(&nc));

                if let Some(b) = bests {
                    if b.cmp(&nc) == Ordering::Equal {
                        // this looks like the right path
                        let mut next_path = next.clone();
                        next_path.push(nc);
                        q.push_back(next_path);
                    }
                }
            }
        }

        paths
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
