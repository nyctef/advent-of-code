use derive_more::Constructor;
use itertools::Itertools;
use std::fmt::Display;

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

    fn advance(&mut self) {
        assert!(self.dot < self.rule.expansion.len());
        self.dot += 1
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
    col_token: &'i str,
}

impl<'r, 'i> Column<'r, 'i> {
    fn new(index: usize, token: &'i str) -> Self {
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
        // TODO: in this implementation the first column takes ownership
        // of the state, but presumably states move between columns
        // as they progress - how will that work?
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
            columns: input_tokens
                .iter()
                .enumerate()
                .map(|(i, &t)| Column::new(i, t))
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
    }

    /// TODO: I think the 'g:'i bound is backwards? we need the input string to outlive the
    /// rules/grammar surely
    fn predict<'r, 'g: 'i>(
        grammar: &'g Grammar<'i>,
        col: &mut Column<'r, 'i>,
        to_predict: &Term<'i>,
        _state: &mut State,
    ) {
        for alt in grammar.rules_for(to_predict) {
            col.add(State::new(&alt, col.col_index))
        }
        /*
        if sym in epsilon: _state.advance()
        */
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

        state.advance();

        assert_eq!(format!("{:?}", state), "<A> => ['a'] | ['b'] (0, None)");
    }
}
