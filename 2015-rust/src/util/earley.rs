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

#[derive(Debug)]
struct Chart<'r, 'i> {
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
                .map(|(i, Some(&t))| Column::new(i, Some(t)))
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

    pub fn run(&self, input_tokens: &[&'i str]) -> () {
        let mut chart = Chart::from_tokens(input_tokens);

        // slightly nonstandard: like in the gopinath.org article, we allow multiple
        // start rules for a given starting token, rather than requiring a unique starting rule
        for alt in self.grammar.rules_for(&self.grammar.start) {
            chart.columns[0].add(State::new(alt, 0))
        }

        let num_columns = chart.columns.len();
        for c in 0..num_columns {
            let col = &mut chart.columns[c];

            // slightly weird way to loop over states in the column, since `predict`
            // can keep adding more states in front of us
            let mut s = 0;
            while s < col.states.len() {
                let state = col.states[s].clone();
                if state.finished() {
                    Self::complete(&mut chart, &state, col);
                } else {
                    let sym = state
                        .at_dot()
                        .expect("unfinished state should have term at dot");
                    match sym {
                        Term::Nonterminal(_) => Self::predict(&self.grammar, col, sym, &state),
                        Term::Terminal(t) => {
                            if c + 1 < num_columns {
                                Self::scan(&mut chart.columns[c + 1], &state, t)
                            }
                        }
                    }
                }
                s += 1;
            }
        }
    }

    /// one of our states has arrived at a nonterminal `to_predict`, so add its expansions
    /// to the current column.
    ///
    /// TODO: I think the 'g:'i bound is backwards? we need the input string to outlive the
    /// rules/grammar surely
    fn predict<'r, 'g: 'i>(
        grammar: &'g Grammar<'i>,
        col: &mut Column<'r, 'i>,
        to_predict: &Term<'i>,
        _state: &State,
    ) {
        for alt in grammar.rules_for(to_predict) {
            col.add(State::new(&alt, col.col_index))
        }
        /*
        if sym in epsilon: _state.advance()
        */
    }

    /// if the next column's token matches a terminal we're expecting, then we can
    /// advance a state and move it to the next column
    fn scan<'r>(col: &mut Column<'r, 'i>, state: &State<'r, 'i>, token: &'i str) {
        if Some(token) == col.col_token {
            col.add(state.advance());
        }
    }

    /// if we've finished matching a rule <X> => ...| then go back and find all the
    /// parent rules that were trying to match an <X> and advance those by one.
    ///
    /// The advanced states get added to the current column since that's how far we've
    /// progressed through the string at the point the advancement happens.
    fn complete<'r>(
        chart: &mut Chart<'r, 'i>,
        state: &State<'r, 'i>,
        current_col: &mut Column<'r, 'i>,
    ) {
        let starting_col = &mut chart.columns[state.start_col];
        let parent_states = starting_col
            .states
            .iter()
            .filter(|s| s.at_dot() == Some(state.name()));

        for st in parent_states {
            current_col.add(st.advance());
        }
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
}
