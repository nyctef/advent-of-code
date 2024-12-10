use derive_more::Constructor;
use itertools::Itertools;
use std::{fmt::Display, iter};

// TODO: make this generic in the token type rather than assuming `&'input str` ?

// largely following https://rahul.gopinath.org/post/2021/02/06/earley-parsing/
// also using some of the ideas from https://loup-vaillant.fr/tutorials/earley-parsing/parser

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
pub enum Term<'i> {
    Terminal(&'i str),
    Nonterminal(&'i str),
}

impl Display for Term<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Terminal(t) => f.write_fmt(format_args!("'{}'", t)),
            Term::Nonterminal(nt) => f.write_fmt(format_args!("<{}>", nt)),
        }
    }
}

impl std::fmt::Debug for Term<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self))
    }
}

impl<'i> Term<'i> {
    pub fn get_value(&self) -> &'i str {
        match self {
            Term::Terminal(t) => t,
            Term::Nonterminal(nt) => nt,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Constructor)]
pub struct Rule<'i> {
    pub matches: Term<'i>,
    pub expansion: Vec<Term<'i>>,
}

impl std::fmt::Debug for Rule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?} => {:?}", self.matches, self.expansion))
    }
}

#[derive(Clone)]
struct State<'r, 'i> {
    rule: &'r Rule<'i>,
    dot: usize,
    start_col: usize,
    end_col: Option<usize>,
}

impl std::fmt::Debug for State<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{{ {:?} => {} |{} ({}, {}) }}",
            self.rule.matches,
            &self.rule.expansion[..self.dot]
                .iter()
                .map(|e| format!("{:?}", e))
                .join(" "),
            &self.rule.expansion[self.dot..]
                .iter()
                .map(|e| format!("{:?}", e))
                .join(" "),
            self.start_col,
            self.end_col
                .map(|e| format!("{}", e))
                .unwrap_or("?".to_owned())
        ))
    }
}

impl<'r, 'i> State<'r, 'i> {
    fn new(rule: &'r Rule<'i>, start_col: usize) -> Self {
        Self {
            rule,
            dot: 0,
            start_col,
            end_col: None,
        }
    }

    fn advance(&self) -> Self {
        assert!(self.dot < self.rule.expansion.len());
        Self {
            dot: self.dot + 1,
            ..*self
        }
    }

    fn finished(&self) -> bool {
        self.dot >= self.rule.expansion.len()
    }

    fn at_dot(&self) -> Option<&'i Term<'r>> {
        if self.dot < self.rule.expansion.len() {
            Some(&self.rule.expansion[self.dot])
        } else {
            None
        }
    }

    /// TODO: better name than `name`?
    fn name(&self) -> &'r Term<'i> {
        &self.rule.matches
    }

    /// Used for equality/hashing/etc.
    /// Note this tuple doesn't take the end col into account -
    /// TODO verify this is what we want
    fn as_tuple(&self) -> (&'r Rule<'i>, usize, usize) {
        (self.rule, self.dot, self.start_col)
    }
}

impl Eq for State<'_, '_> {}
impl PartialEq for State<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_tuple().eq(&other.as_tuple())
    }
}
impl std::hash::Hash for State<'_, '_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_tuple().hash(state)
    }
}

#[derive(Debug)]
struct Column<'r, 'i> {
    states: Vec<State<'r, 'i>>,
    col_index: usize,
    col_token: Option<&'i str>,
}

impl<'r, 'i> Column<'r, 'i> {
    fn new(index: usize, token: Option<&'i str>) -> Self {
        Self {
            states: vec![],
            col_index: index,
            col_token: token,
        }
    }

    fn add(&mut self, mut state: State<'r, 'i>) {
        // TODO: it might become worth storing a separate hashset
        // of seen states here - we'd need to be careful of mutating
        // states while they're contained in the hashset, though.
        if !self.states.contains(&state) {
            state.end_col = Some(self.col_index);
            self.states.push(state);
        }
    }
}

// TODO: make chart private and return a parse tree instead
#[derive(Debug)]
pub struct Chart<'r, 'i> {
    columns: Vec<Column<'r, 'i>>,
}

impl<'i> Chart<'_, 'i> {
    fn from_tokens(input_tokens: &[&'i str]) -> Self {
        Self {
            // initial column 0 is for the start state without
            // any characters processed.
            // total length of the chart is input_tokens.len() + 1
            columns: iter::once(None)
                .chain(input_tokens.iter().map(Some))
                .enumerate()
                // TODO: nicer way to convert from Option<&&_> to Option<&_>?
                .map(|(i, t)| Column::new(i, t.copied()))
                .collect_vec(),
        }
    }

    /// Once we've finished recognising rules, we can remove any unfinished
    /// states since they won't be useful for constructing a parse tree.
    fn retain_finished(&mut self) {
        for col in &mut self.columns {
            col.states.retain(|s| s.finished());
        }
    }
}

#[derive(Debug, Constructor)]
pub struct Grammar<'i> {
    rules: Vec<Rule<'i>>,
    start: Term<'i>,
}

impl<'i> Grammar<'i> {
    fn rules_for(&self, term: &Term<'i>) -> impl Iterator<Item = &Rule<'i>> {
        assert!(matches!(term, Term::Nonterminal(_)));
        // TODO: can we remove this clone?
        let term = *term;
        self.rules.iter().filter(move |r| r.matches == term)
    }

    pub fn from_rules(rules: impl IntoIterator<Item = Rule<'i>>, start: Term<'i>) -> Self {
        // TODO: calculate epsilon here if we ever need to handle nullable rules
        Grammar::new(rules.into_iter().collect_vec(), start)
    }
}

#[derive(Debug)]
pub struct Parser {}

impl Parser {
    // pub fn from_rules(rules: impl IntoIterator<Item = Rule<'i>>, start: Term<'i>) -> Self {
    //     Self {
    //         // TODO: calculate epsilon here if we ever need to handle nullable rules
    //         grammar: Grammar::new(rules.into_iter().collect_vec(), start),
    //     }
    // }

    pub fn run<'i>(grammar: &Grammar<'i>, input_tokens: &[&'i str]) -> usize {
        let mut chart = Chart::from_tokens(input_tokens);

        // slightly nonstandard: like in the gopinath.org article, we allow multiple
        // start rules for a given starting token, rather than requiring a unique starting rule
        for alt in grammar.rules_for(&grammar.start) {
            chart.columns[0].add(State::new(alt, 0))
        }

        let num_columns = chart.columns.len();
        for c in 0..num_columns {
            println!("running column {c}");
            // some messy code with split_at_mut to be able to get mutable references
            // to different vec entries at the same time.
            //
            // `prev_cols` ends up containing all the columns up to (but not including)
            // the current chart column we're looking at. This should preserve column
            // indexes, so indexing into this with `state.start_col` should do the right
            // thing. Fortunately we only need to reference one of these columns at a
            // time, so we don't need to do further splitting here.
            //
            // `col` ends up containing a mutable reference just to the current column
            // we're looking at.
            //
            // `next_col` has an optional reference to col + 1, used for advancing states
            let (prev_cols, these_cols) = chart.columns.split_at_mut(c);
            let (col, next_cols) = these_cols.split_at_mut(1);
            let col = &mut col[0];
            let mut next_col = next_cols.get_mut(0);

            // slightly weird way to loop over states in the column, since `predict`
            // can keep adding more states in front of us
            let mut s = 0;
            while s < col.states.len() {
                let state = col.states[s].clone();
                // println!("testing state {state:?}");
                if state.finished() {
                    // if we've finished matching a rule <X> => ...| then go back and find all the
                    // parent rules that were trying to match an <X> and advance those by one.
                    //
                    // The advanced states get added to the current column since that's how far we've
                    // progressed through the string at the point the advancement happens.
                    let starting_col = &mut prev_cols[state.start_col];
                    let parent_states = starting_col
                        .states
                        .iter()
                        .filter(|s| s.at_dot() == Some(state.name()));

                    for st in parent_states {
                        col.add(st.advance());
                    }
                } else {
                    let sym = state
                        .at_dot()
                        .expect("unfinished state should have term at dot");
                    match sym {
                        Term::Nonterminal(_) => {
                            // one of our states has arrived at a nonterminal `to_predict`, so add its expansions
                            // to the current column.
                            // println!("adding expansions for {sym:?} to col");
                            for alt in grammar.rules_for(sym) {
                                col.add(State::new(alt, col.col_index))
                            }
                            /*
                            if sym in epsilon: _state.advance()
                            */
                        }
                        Term::Terminal(t) => {
                            if let Some(ref mut next_col) = next_col {
                                // if the next column's token matches a terminal we're expecting, then we can
                                // advance a state and move it to the next column
                                // println!(
                                //     "checking current token {} against next col token {:?}",
                                //     t, next_col.col_token
                                // );
                                if Some(*t) == next_col.col_token {
                                    next_col.add(state.advance());
                                }
                            }
                        }
                    }
                }
                s += 1;
            }
        }

        chart.retain_finished();

        // now we need to extract the possible parse trees from the chart, and find the shortest

        // the first step is to find all the rules from the starting token that have matched the
        // entire string
        let final_states = &chart
            .columns
            .last()
            .unwrap()
            .states
            .iter()
            .filter(|s| s.rule.matches == grammar.start && s.finished() && s.start_col == 0)
            .collect_vec();

        let forest = Self::parse_forest(&chart, final_states);
        // dbg!(&forest);
        let trees = Self::extract_trees(&chart, forest);
        // dbg!(&trees);

        count_expansions_in_tree(&trees[0])
    }

    // https://rahul.gopinath.org/post/2021/02/06/earley-parsing/#parse_paths
    //
    // named_expr is a list of terms that we know matches the input string between `from` and
    // `until`.
    //
    // we pop one term off the end of this list:
    //  - if it's a terminal, we can just check it directly against the input string at `until`
    //  - if it's a nonterminal, we find all the states ending at `until` which were parsing that
    //    nonterminal, and record their start positions. These are the possible end positions for
    //    the remaining prefix of `named_expr`
    fn parse_paths<'r, 'c, 'i>(
        named_expr: &'r [Term<'i>],
        chart: &'c Chart<'r, 'i>,
        from: usize,
        until: usize,
    ) -> Vec<Vec<StateOrTerminal<'c, 'r, 'i>>> {
        // eprintln!("parse_paths({:?}, {}, {})", named_expr, from, until);
        let paths = |state, start: usize, remaining_prefix: &'r [Term<'i>]| {
            // dbg!(&state, &remaining_prefix, start);
            if remaining_prefix.is_empty() {
                // base case: we've run out of elements from `named_expr` to match
                if start == from {
                    vec![vec![state]]
                } else {
                    // we failed to match the rule properly, so this path isn't valid
                    vec![]
                }
            } else {
                // recursive case: return the element we just matched, plus all the possible
                // paths for the remaining elements
                let mut result = vec![];
                for possible_remaining_path in
                    Self::parse_paths(remaining_prefix, chart, from, start)
                {
                    let mut path = vec![state];
                    path.extend(possible_remaining_path);
                    result.push(path);
                }
                result
            }
        };

        let (remaining, last) = named_expr.split_at(named_expr.len() - 1);
        let last = last[0];
        let starts = match last {
            // if the last element of the expression is a terminal, then we just check
            // it against the column directly
            Term::Terminal(t) => {
                // only column 0 has a None col_token
                if chart.columns[until].col_token == Some(t) {
                    vec![(StateOrTerminal::Terminal(t), until - 1)]
                } else {
                    vec![]
                }
            }
            // if the last element is a nonterminal, then we need to find all the states
            // completing that nonterminal at `until`
            Term::Nonterminal(_) => chart.columns[until]
                .states
                .iter()
                .filter_map(|s| {
                    if s.rule.matches == last && s.finished() {
                        Some((StateOrTerminal::State(s), s.start_col))
                    } else {
                        None
                    }
                })
                .collect_vec(),
        };
        // dbg!(&starts);

        
        // dbg!(&paths);
        starts
            .into_iter()
            .flat_map(|(s, start)| paths(s, start, remaining))
            .collect_vec()
    }

    // TODO: docs
    fn forest<'c, 'r, 'i>(
        chart: &'c Chart<'r, 'i>,
        s: StateOrTerminal<'c, 'r, 'i>,
    ) -> ForestNode<'c, 'r, 'i> {
        match s {
            StateOrTerminal::State(state) => (s, Self::parse_forest(chart, &[state]).1),
            StateOrTerminal::Terminal(_) => (s, vec![]),
        }
    }

    // TODO: docs
    fn _parse_forest<'c, 'r, 'i>(
        chart: &'c Chart<'r, 'i>,
        state: &'c State<'r, 'i>,
    ) -> (&'c Term<'i>, Vec<Vec<StateOrTerminal<'c, 'r, 'i>>>) {
        let named_expr = &state.rule.expansion;
        let pathexprs =
            Self::parse_paths(named_expr, chart, state.start_col, state.end_col.unwrap());
        (
            &state.rule.matches,
            pathexprs
                .into_iter()
                .map(|p| p.into_iter().rev().collect_vec())
                .collect_vec(),
        )
    }

    // https://rahul.gopinath.org/post/2021/02/06/earley-parsing/#parse_forest
    //
    // note this only gives us one layer of the forest at a time (?)
    fn parse_forest<'c, 'r, 'i>(
        chart: &'c Chart<'r, 'i>,
        states: &[&'c State<'r, 'i>],
    ) -> ForestNode<'c, 'r, 'i> {
        assert!(!states.is_empty());
        assert!(states.iter().all(|s| s.finished()));
        assert!(states
            .iter()
            .all(|s| s.rule.matches == states[0].rule.matches));

        let forest = states.iter().flat_map(|s| Self::_parse_forest(chart, s).1);

        (StateOrTerminal::State(states[0]), forest.collect_vec())
    }

    fn extract_a_tree<'c, 'r, 'i>(
        chart: &'c Chart<'r, 'i>,
        forest_node: ForestNode<'c, 'r, 'i>,
    ) -> TreeNode<'c, 'r, 'i> {
        let (name, paths) = forest_node;
        if paths.is_empty() {
            return TreeNode::empty(name);
        }

        TreeNode(
            name,
            paths[0]
                .iter()
                .map(|p| Self::extract_a_tree(chart, Self::forest(chart, *p)))
                .collect_vec(),
        )
    }

    fn extract_trees<'c, 'r, 'i>(
        chart: &'c Chart<'r, 'i>,
        forest: ForestNode<'c, 'r, 'i>,
    ) -> Vec<TreeNode<'c, 'r, 'i>> {
        vec![Self::extract_a_tree(chart, forest)]
    }
}

fn count_expansions_in_tree(tree: &TreeNode<'_, '_, '_>) -> usize {
    let TreeNode(node, branches) = tree;
    let mut sum = branches.iter().map(count_expansions_in_tree).sum();
    if let StateOrTerminal::State(s) = node {
        // we count the rule if it expands to more than one token, or if it expands to a nonterminal
        // the rules that we added which just turn a single nonterminal into the equivalent terminal
        // are our own addition and shouldn't be counted

        // eprintln!("inspecting {:?}", s);

        if s.rule.expansion.len() != 1 || !matches!(s.rule.expansion[0], Term::Terminal(_)) {
            // eprintln!(" -> it counts");
            sum += 1;
        }
    }
    sum
}

// a "forest" is a group of possible parse trees (since the grammar is ambiguous)
type ForestNode<'c, 'r, 'i> = (
    StateOrTerminal<'c, 'r, 'i>,
    Vec<Vec<StateOrTerminal<'c, 'r, 'i>>>,
);

// a "tree" is one particular parse tree extracted from the forest
#[derive(Debug)]
#[allow(dead_code)]
struct TreeNode<'c, 'r, 'i>(StateOrTerminal<'c, 'r, 'i>, Vec<TreeNode<'c, 'r, 'i>>);

impl<'c, 'r, 'i> TreeNode<'c, 'r, 'i> {
    pub fn empty(s: StateOrTerminal<'c, 'r, 'i>) -> Self {
        Self(s, vec![])
    }
}

// TODO: do we need this lifetime 'c for borrowing from the chart?
#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
enum StateOrTerminal<'c, 'r, 'i> {
    State(&'c State<'r, 'i>),
    Terminal(&'i str),
}

#[cfg(test)]
mod test {
    use super::Term::*;
    use super::*;

    #[test]
    fn basic_state_tests() {
        let rule = Rule::new(Nonterminal("A"), vec![Terminal("a"), Terminal("b")]);
        let mut state = State::new(&rule, 0);

        assert_eq!(format!("{:?}", state), "<A> => [] | ['a', 'b'] (0, None)");

        state = state.advance();

        assert_eq!(format!("{:?}", state), "<A> => ['a'] | ['b'] (0, None)");
    }

    #[test]
    fn test_basic_example_1() {
        let rules = vec![Rule::new(
            Nonterminal("S"),
            vec![Terminal("a"), Terminal("b"), Terminal("c")],
        )];
        let grammar = Grammar::from_rules(rules, Nonterminal("S"));
        let result = Parser::run(&grammar, &["a", "b", "c"]);

        println!("{:?}", result);
        todo!()
    }

    #[test]
    fn test_basic_example_2() {
        // S -> A A
        // A -> a B
        // B -> b
        let rules = vec![
            Rule::new(Nonterminal("S"), vec![Nonterminal("A"), Nonterminal("A")]),
            Rule::new(Nonterminal("A"), vec![Terminal("a"), Nonterminal("B")]),
            Rule::new(Nonterminal("B"), vec![Terminal("b")]),
        ];
        let grammar = Grammar::from_rules(rules, Nonterminal("S"));
        let result = Parser::run(&grammar, &["a", "b", "a", "b"]);

        println!("{:?}", result);
        todo!()
    }
}
