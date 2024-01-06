use derive_more::Constructor;
use std::fmt::Display;

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

#[derive(Eq, PartialEq, Constructor)]
pub struct Rule<'i> {
    pub matches: Term<'i>,
    pub expansion: Vec<Term<'i>>,
}

impl std::fmt::Debug for Rule<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?} => {:?}", self.matches, self.expansion))
    }
}
