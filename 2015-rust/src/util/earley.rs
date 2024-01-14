use derive_more::Constructor;
use itertools::Itertools;
use std::{fmt::Display, iter};

// TODO: make this generic in the token type rather than assuming `&'input str` ?

// largely following https://rahul.gopinath.org/post/2021/02/06/earley-parsing/

#[derive(Eq, PartialEq, Hash, Clone)]
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
        f.write_fmt(format_args!("{}", self.to_string()))
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
            "{:?} => {:?} | {:?} ({}, {:?})",
            self.rule.matches,
            &self.rule.expansion[..self.dot],
            &self.rule.expansion[self.dot..],
            self.start_col,
            self.end_col
        ))
    }
}

impl<'i, 'r> State<'i, 'r> {
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

    fn at_dot(&self) -> Option<&'r Term<'i>> {
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

impl<'i, 'r> Eq for State<'i, 'r> {}
impl<'i, 'r> PartialEq for State<'i, 'r> {
    fn eq(&self, other: &Self) -> bool {
        self.as_tuple().eq(&other.as_tuple())
    }
}
impl<'i, 'r> std::hash::Hash for State<'i, 'r> {
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

impl<'r, 'i> Chart<'r, 'i> {
    fn from_tokens(input_tokens: &[&'i str]) -> Self {
        Self {
            // initial column 0 is for the start state without
            // any characters processed.
            // total length of the chart is input_tokens.len() + 1
            columns: iter::once(None)
                .chain(input_tokens.iter().map(|t| Some(t)))
                .enumerate()
                // TODO: nicer way to convert from Option<&&_> to Option<&_>?
                .map(|(i, t)| Column::new(i, t.map(|x| *x)))
                .collect_vec(),
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
        let term = term.clone();
        self.rules.iter().filter(move |r| r.matches == term)
    }
}

#[derive(Debug)]
pub struct Parser<'i> {
    grammar: Grammar<'i>,
}

impl<'i> Parser<'i> {
    pub fn from_rules(rules: impl IntoIterator<Item = Rule<'i>>, start: Term<'i>) -> Self {
        Self {
            // TODO: calculate epsilon here if we ever need to handle nullable rules
            grammar: Grammar::new(rules.into_iter().collect_vec(), start),
        }
    }

    pub fn run(&self, input_tokens: &[&'i str]) -> Chart {
        let mut chart = Chart::from_tokens(input_tokens);

        // slightly nonstandard: like in the gopinath.org article, we allow multiple
        // start rules for a given starting token, rather than requiring a unique starting rule
        for alt in self.grammar.rules_for(&self.grammar.start) {
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
            let mut next_col = next_cols.iter_mut().nth(0);

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
                            for alt in (&self.grammar).rules_for(sym) {
                                col.add(State::new(&alt, col.col_index))
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

        // now we look for any states in the final column that match the start rule
        // and have consumed the entire input string
        for final_state in &chart.columns.last().unwrap().states {
            if final_state.rule.matches == self.grammar.start
                && final_state.start_col == 0
                && final_state.finished()
            {
                println!("found final state {:?}", final_state);
            }
        }
        chart
    }
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
        let parser = Parser::from_rules(rules, Nonterminal("S"));
        let result = parser.run(&vec!["a", "b", "c"]);

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
        let parser = Parser::from_rules(rules, Nonterminal("S"));
        let result = parser.run(&vec!["a", "b", "a", "b"]);

        println!("{:?}", result);
        todo!()
    }
}
