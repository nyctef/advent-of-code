use derive_more::Constructor;
use itertools::Itertools;
use nom::AsChar;
use std::{
    fmt::Debug,
    ops::{Add, Index, Range},
};

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

    pub fn iter_positions_rc(&self) -> impl Iterator<Item = CharGridIndexRC> + '_ {
        self.enumerate_chars_rc().map(|(pos, _)| pos)
    }

    pub fn enumerate_chars_rc(&self) -> impl Iterator<Item = (CharGridIndexRC, char)> + '_ {
        self.lines.iter().enumerate().flat_map(|(r, line)| {
            line.iter()
                .enumerate()
                .map(move |(c, char)| (CharGridIndexRC::new(r, c), *char))
        })
    }

    pub fn enumerate_numbers(&self) -> impl Iterator<Item = (CharGridRange<CharGridIndexRC>, u32)> {
        let mut s = String::new();
        let mut range_start: Option<CharGridIndexRC> = None;
        // TODO: might be interesting to try and make this lazy using something like iter::from_fn ?
        let mut result = Vec::new();
        let mut last_pos = CharGridIndexRC::zero();
        for (pos, char) in self.enumerate_chars_rc() {
            // dbg!((pos, char, &s));
            let row_changed = pos.row != last_pos.row;
            let end_of_digits = (!char.is_digit(10) || row_changed) && !s.is_empty();

            if end_of_digits {
                let range_end = last_pos.right();
                result.push((
                    CharGridRange::new(range_start.unwrap(), range_end),
                    s.parse().unwrap(),
                ));

                s.clear();
                range_start = None;
            }

            if char.is_digit(10) {
                s.push(char);
                if range_start == None {
                    range_start = Some(pos)
                }
            }
            last_pos = pos;
        }

        result.into_iter()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Constructor)]
pub struct CharGridIndexRC {
    pub row: usize,
    pub col: usize,
}

impl CharGridIndexRC {
    pub fn zero() -> CharGridIndexRC {
        CharGridIndexRC { row: 0, col: 0 }
    }

    pub fn right(&self) -> CharGridIndexRC {
        CharGridIndexRC {
            row: self.row,
            col: self.col + 1,
        }
    }
}

impl Add<usize> for CharGridIndexRC {
    type Output = CharGridIndexRC;

    fn add(self, rhs: usize) -> Self::Output {
        Self {
            col: self.col + rhs,
            row: self.row + rhs,
        }
    }
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

/**
using our own range type, since rust's isn't really flexible in the way we want
- https://kaylynn.gay/blog/post/rust_ranges_and_suffering
- https://ridiculousfish.com/blog/posts/least-favorite-rust-type.html
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq, Constructor)]
pub struct CharGridRange<T> {
    pub start: T,
    /// exclusive, so may point just off the end of the grid
    pub end: T,
}
