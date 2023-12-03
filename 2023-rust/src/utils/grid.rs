use itertools::Itertools;
use nom::AsChar;
use std::{fmt::Debug, ops::Index};

pub struct CharGrid {
    width: usize,
    height: usize,
    // note that this is pretty inefficient - `char` is 32 bits
    // but we're using it to store only 7-bit ascii values.
    //
    // however, this seems to be the only way we can hand out references
    // to `char`s in the Index trait implementations :/
    //
    // (if we just store a vec of strings, then there doesn't seem to be
    // a safe way to reference into the string data and get a char out
    // without some really bad hacks like keeping a separate cache of `char`
    // values)
    lines: Vec<Vec<char>>,
}

impl CharGrid {
    pub fn new(lines: Vec<String>) -> CharGrid {
        let height = lines.len();
        assert!(height > 0);
        let width = lines[0].len();
        assert!(width > 0);
        assert!(
            lines.iter().all(|l| l.len() == width),
            "all lines must have same length"
        );
        assert!(lines.iter().all(|l| l.chars().all(|c| c.is_ascii())));

        let lines = lines.iter().map(|l| l.chars().collect_vec()).collect_vec();

        CharGrid {
            width,
            height,
            lines,
        }
    }

    pub fn from_string(input: &str) -> CharGrid {
        Self::new(
            input
                .trim()
                .lines()
                .to_owned()
                .map(|l| l.to_owned())
                .collect_vec(),
        )
    }

    pub fn height(&self) -> usize {
        self.height
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn lines(&self) -> impl Iterator<Item = String> + '_ {
        self.lines.iter().map(|l| l.iter().collect::<String>())
    }

    pub fn index_rc(&self, row: usize, col: usize) -> char {
        assert!(row < self.height);
        assert!(col < self.width);
        self.lines[row][col].as_char()
    }
}

impl Debug for CharGrid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "CharGrid width: {} height: {} lines:\n{}\n",
            &self.width,
            &self.height,
            &self.lines().join("\n")
        ))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharGridIndexRC {
    pub row: usize,
    pub col: usize,
}

impl Index<CharGridIndexRC> for CharGrid {
    type Output = char;

    fn index(&self, index: CharGridIndexRC) -> &Self::Output {
        let CharGridIndexRC { row, col } = index;
        assert!(row < self.height);
        assert!(col < self.width);
        &self.lines[row][col]
    }
}
