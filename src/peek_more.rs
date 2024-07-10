use std::{collections::VecDeque, str::Chars};

pub struct PeekMore<'a> {
    inner: Chars<'a>,
    peeked: VecDeque<char>,
    peek_index: usize,
}

impl<'a> PeekMore<'a> {
    pub fn peek(&mut self) -> Option<char> {
        let index = if self.peek_index > 0 && self.peeked.is_empty() {
            if self.peek_index > self.peeked.len() {
                self.peek_index = self.peeked.len();
            }
            self.peeked.len() - self.peek_index
        } else {
            let item = self.inner.next()?;
            self.peeked.push_back(item);
            self.peeked.len() - 1
        };

        // Item was either:
        //  - Checked to exist
        //  - Or was just added
        let peek = self.peeked.get(index).expect("Item is known to exist");

        if self.peek_index > 0 {
            self.peek_index -= 1;
        }
        Some(*peek)
    }

    pub fn back(&mut self) -> bool {
        if self.peek_index + 1 > self.peeked.len() {
            return false;
        }
        self.peek_index += 1;
        true
    }
}

impl<'a> Iterator for PeekMore<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        self.peek_index = self.peeked.len();
        if let Some(item) = self.peeked.pop_front() {
            return Some(item);
        }
        self.inner.next()
    }
}

impl<'a> From<Chars<'a>> for PeekMore<'a> {
    fn from(inner: Chars<'a>) -> Self {
        Self {
            inner,
            peeked: VecDeque::new(),
            peek_index: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_more_works() {
        let mut it = PeekMore::from("".chars());
        assert_eq!(it.next(), None);
        assert_eq!(it.peek(), None);
        let mut it = PeekMore::from("".chars());
        assert_eq!(it.peek(), None);
        assert_eq!(it.next(), None);
        let mut it = PeekMore::from("abcdef".chars());
        assert_eq!(it.peek(), Some('a'));
        assert_eq!(it.peek(), Some('b'));
        assert_eq!(it.peek(), Some('c'));
        assert_eq!(it.peek(), Some('d'));
        assert_eq!(it.peek(), Some('e'));
        assert_eq!(it.peek(), Some('f'));
        assert_eq!(it.peek(), None);
        assert_eq!(it.next(), Some('a'));
        assert_eq!(it.next(), Some('b'));
        assert_eq!(it.next(), Some('c'));
        assert_eq!(it.next(), Some('d'));
        assert_eq!(it.next(), Some('e'));
        assert_eq!(it.next(), Some('f'));
        assert_eq!(it.peek(), None);
        assert_eq!(it.next(), None);
        let mut it = PeekMore::from("abcdef".chars());
        assert_eq!(it.next(), Some('a'));
        assert_eq!(it.next(), Some('b'));
        assert_eq!(it.peek(), Some('c'));
        assert_eq!(it.next(), Some('c'));
        assert_eq!(it.peek(), Some('d'));
        assert_eq!(it.peek(), Some('e'));
        assert_eq!(it.peek(), Some('f'));
        assert_eq!(it.next(), Some('d'));
        assert_eq!(it.next(), Some('e'));
        assert_eq!(it.next(), Some('f'));
        assert_eq!(it.peek(), None);
        assert_eq!(it.next(), None);
        let mut it = PeekMore::from("abcdef".chars());
        assert_eq!(it.next(), Some('a'));
        assert_eq!(it.peek(), Some('b'));
        assert_eq!(it.peek(), Some('c'));
        assert_eq!(it.next(), Some('b'));
        assert_eq!(it.peek(), Some('c'));
        assert_eq!(it.peek(), Some('d'));
        let mut it = PeekMore::from("abcdef".chars());
        assert_eq!(it.next(), Some('a'));
        assert_eq!(it.peek(), Some('b'));
        assert_eq!(it.peek(), Some('c'));
        assert_eq!(it.next(), Some('b'));
        assert_eq!(it.peek(), Some('c'));
        assert_eq!(it.peek(), Some('d'));
        assert_eq!(it.peek(), Some('e'));
        assert_eq!(it.peek(), Some('f'));
        assert_eq!(it.next(), Some('c'));
        assert_eq!(it.next(), Some('d'));
    }
}
