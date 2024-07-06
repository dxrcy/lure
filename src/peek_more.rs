use std::collections::VecDeque;

pub struct PeekMore<T>
where
    T: Iterator,
{
    inner: T,
    peeked: VecDeque<T::Item>,
}

impl<T> PeekMore<T>
where
    T: Iterator,
{
    pub fn peek(&mut self) -> Option<&T::Item> {
        println!("{}", self.peeked.len());
        let item = self.inner.next()?;
        self.peeked.push_back(item);
        let peek = self.peeked.get(self.peeked.len() - 1).unwrap();
        Some(peek)
    }
}

impl<T> Iterator for PeekMore<T>
where
    T: Iterator,
{
    type Item = T::Item;

    fn next(&mut self) -> Option<T::Item> {
        println!("{}", self.peeked.len());
        if let Some(item) = self.peeked.pop_front() {
            return Some(item);
        }
        self.inner.next()
    }
}

impl<T> From<T> for PeekMore<T>
where
    T: Iterator,
{
    fn from(inner: T) -> Self {
        Self {
            inner,
            peeked: VecDeque::new(),
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
        assert_eq!(it.peek(), Some(&'a'));
        assert_eq!(it.peek(), Some(&'b'));
        assert_eq!(it.peek(), Some(&'c'));
        assert_eq!(it.peek(), Some(&'d'));
        assert_eq!(it.peek(), Some(&'e'));
        assert_eq!(it.peek(), Some(&'f'));
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
        assert_eq!(it.peek(), Some(&'c'));
        assert_eq!(it.next(), Some('c'));
        assert_eq!(it.peek(), Some(&'d'));
        assert_eq!(it.peek(), Some(&'e'));
        assert_eq!(it.next(), Some('d'));
        assert_eq!(it.next(), Some('e'));
        assert_eq!(it.next(), Some('f'));
        assert_eq!(it.peek(), None);
        assert_eq!(it.next(), None);
        let mut it = PeekMore::from("abcdef".chars());
        assert_eq!(it.next(), Some('a'));
        assert_eq!(it.peek(), Some(&'b'));
        assert_eq!(it.peek(), Some(&'c'));
        assert_eq!(it.next(), Some('b'));
        assert_eq!(it.peek(), Some(&'d'));
        assert_eq!(it.peek(), Some(&'e'));
    }
}
