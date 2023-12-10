use derive_more::Constructor;
use itertools::Itertools;
use std::{
    fmt::Debug,
    ops::{Add, Index, Sub},
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

#[allow(dead_code)]
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
        self.lines[row][col]
    }

    pub fn index(&self, index: CharGridIndexRC) -> char {
        self.index_rc(index.row, index.col)
    }

    pub fn iter_positions_rc(&self) -> impl Iterator<Item = CharGridIndexRC> + '_ {
        self.enumerate_chars_rc().map(|(pos, _)| pos)
    }

    pub fn enumerate_chars_rc(&self) -> impl Iterator<Item = (CharGridIndexRC, char)> + Debug + '_ {
        self.lines.iter().enumerate().flat_map(|(r, line)| {
            line.iter()
                .enumerate()
                .map(move |(c, char)| (CharGridIndexRC::new(r, c), *char))
        })
    }

    pub fn enumerate_4_neighbors(
        &self,
        index: CharGridIndexRC,
    ) -> impl Iterator<Item = (CharGridIndexRC, char)> + Debug + '_ {
        let mut result = vec![];
        for n in [index.left(), index.up()] {
            if let Some(n) = n {
                result.push((n, self.index(n)));
            }
        }

        for n in [index.right(), index.down()] {
            if self.is_in_bounds(n) {
                result.push((n, self.index(n)));
            }
        }
        result.into_iter()
    }

    pub fn is_in_bounds(&self, index: CharGridIndexRC) -> bool {
        // we know that row/col are >= 0 because they're stored as unsigned types
        index.row < self.height && index.col < self.width
    }

    pub fn enumerate_range_rc(
        &self,
        range: CharGridRange<CharGridIndexRC>,
    ) -> impl Iterator<Item = (CharGridIndexRC, char)> {
        // todo: should this use an iter() on the range itself?
        let range = self.clip_range(range);
        let mut result = Vec::new();
        for r in range.start.row..range.end.row {
            for c in range.start.col..range.end.col {
                result.push((CharGridIndexRC::new(r, c), self.lines[r][c]))
            }
        }
        result.into_iter()
    }

    pub fn clip_range(
        &self,
        range: CharGridRange<CharGridIndexRC>,
    ) -> CharGridRange<CharGridIndexRC> {
        CharGridRange {
            start: self.clip_index(range.start),
            end: self.clip_index(range.end),
        }
    }

    pub fn clip_index(&self, index: CharGridIndexRC) -> CharGridIndexRC {
        CharGridIndexRC {
            row: index.row.clamp(0, self.height),
            col: index.col.clamp(0, self.width),
        }
    }

    pub fn set_range_rc(&mut self, range: CharGridRange<CharGridIndexRC>, new_value: char) {
        for (pos, _) in self.enumerate_range_rc(range) {
            self.lines[pos.row][pos.col] = new_value;
        }
    }

    pub fn enumerate_numbers(&self) -> impl Iterator<Item = (CharGridRange<CharGridIndexRC>, u32)> {
        let mut s = String::new();
        let mut range_start: Option<CharGridIndexRC> = None;
        // TODO: might be interesting to try and make this lazy using something like iter::from_fn ?
        // (or just fold over enumerate_chars_rc())
        let mut result = Vec::new();
        let mut last_pos = CharGridIndexRC::zero();
        for (pos, char) in self.enumerate_chars_rc() {
            // dbg!((pos, char, &s));
            let row_changed = pos.row != last_pos.row;
            let end_of_digits = (!char.is_ascii_digit() || row_changed) && !s.is_empty();

            if end_of_digits {
                let range_end = last_pos + 1;
                result.push((
                    CharGridRange::new(range_start.unwrap(), range_end),
                    s.parse().unwrap(),
                ));

                s.clear();
                range_start = None;
            }

            if char.is_ascii_digit() {
                s.push(char);
                if range_start.is_none() {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Constructor, Hash)]
pub struct CharGridIndexRC {
    pub row: usize,
    pub col: usize,
}

#[allow(dead_code)]
impl CharGridIndexRC {
    pub fn zero() -> CharGridIndexRC {
        CharGridIndexRC { row: 0, col: 0 }
    }

    pub fn right(&self) -> CharGridIndexRC {
        CharGridIndexRC {
            col: self.col + 1,
            ..*self
        }
    }

    /// since we can't store negative indexes
    pub fn left(&self) -> Option<CharGridIndexRC> {
        if self.col == 0 {
            None
        } else {
            Some(CharGridIndexRC {
                col: self.col - 1,
                ..*self
            })
        }
    }

    /// since we can't store negative indexes
    pub fn up(&self) -> Option<CharGridIndexRC> {
        if self.row == 0 {
            None
        } else {
            Some(CharGridIndexRC {
                row: self.row - 1,
                ..*self
            })
        }
    }

    pub fn down(&self) -> CharGridIndexRC {
        CharGridIndexRC {
            row: self.row + 1,
            ..*self
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

impl Sub<usize> for CharGridIndexRC {
    type Output = CharGridIndexRC;

    fn sub(self, rhs: usize) -> Self::Output {
        Self {
            // todo: is it confusing that we don't actually go negative here?
            col: self.col.saturating_sub(rhs),
            row: self.row.saturating_sub(rhs),
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

impl CharGridRange<CharGridIndexRC> {
    pub fn grow_1(&self) -> CharGridRange<CharGridIndexRC> {
        CharGridRange {
            start: self.start - 1,
            end: self.end + 1,
        }
    }
}
