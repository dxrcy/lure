use std::mem;

const BUFFER_SIZE: usize = 4;

/// Keep a buffer of previously iterated items
pub struct Backtrackable<T>
where
    T: Iterator,
    <T as Iterator>::Item: Copy + Default,
{
    source: T,
    buffer: CyclicBuffer<<T as Iterator>::Item>,
    back_count: usize,
}

impl<T> From<T> for Backtrackable<T>
where
    T: Iterator,
    <T as Iterator>::Item: Copy + Default,
{
    fn from(source: T) -> Self {
        Self {
            source,
            buffer: CyclicBuffer::new(),
            back_count: 0,
        }
    }
}

impl<T> Backtrackable<T>
where
    T: Iterator,
    <T as Iterator>::Item: Copy + Default,
{
    /// Go back one step
    ///
    /// The next call to `.next` will yield the same value as previously returned
    pub fn back(&mut self) {
        self.back_count += 1;
        if self.back_count > BUFFER_SIZE {
            panic!("Too many back steps");
        }
    }

    pub fn peek(&mut self) -> Option<<T as Iterator>::Item> {
        let item = self.next()?;
        self.back();
        Some(item)
    }
}

impl<T> Iterator for Backtrackable<T>
where
    T: Iterator,
    <T as Iterator>::Item: Copy + Default,
{
    type Item = <T as Iterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.back_count > 0 {
            let item = self.buffer.get_back(self.back_count - 1).unwrap();
            self.back_count -= 1;
            return Some(item);
        }
        let Some(item) = self.source.next() else {
            return None;
        };
        self.buffer.push(item);
        Some(item)
    }
}

/// A buffer with a maximum length which wraps around, overriding previous values
struct CyclicBuffer<T> {
    buffer: [T; BUFFER_SIZE],
    count: usize,
}

impl<T> CyclicBuffer<T>
where
    T: Copy + Default,
{
    /// Create a new cyclic buffer
    fn new() -> Self {
        let buffer = [Default::default(); BUFFER_SIZE];
        Self { buffer, count: 0 }
    }

    /// Push a new item to the 'end' of the buffer
    fn push(&mut self, value: T) {
        self.buffer[self.count % BUFFER_SIZE] = value;
        self.count += 1;
    }

    /// Get the nth item from the 'end' of the buffer
    fn get_back(&mut self, index: usize) -> Option<T> {
        if index >= BUFFER_SIZE || index >= self.count || self.count == 0 {
            return None;
        }
        let actual_index = (self.count - index - 1) % BUFFER_SIZE;
        Some(self.buffer[actual_index])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn backtrack_works() {
        let string = "abcde";
        let mut chars = Backtrackable::from(string.chars());
        assert_eq!(chars.next(), Some('a'), "1");
        assert_eq!(chars.next(), Some('b'), "2");
        assert_eq!(chars.next(), Some('c'), "3");
        chars.back();
        assert_eq!(chars.next(), Some('c'), "4");
        assert_eq!(chars.next(), Some('d'), "5");
        chars.back();
        chars.back();
        assert_eq!(chars.next(), Some('c'), "6");
        assert_eq!(chars.next(), Some('d'), "7");
        chars.back();
        assert_eq!(chars.next(), Some('d'), "8");
        chars.back();
        chars.back();
        chars.back();
        chars.back(); // 4 is max
        assert_eq!(chars.next(), Some('a'), "9");
        assert_eq!(chars.next(), Some('b'), "10");
        assert_eq!(chars.next(), Some('c'), "11");
        assert_eq!(chars.next(), Some('d'), "12");
        assert_eq!(chars.next(), Some('e'), "13");
        assert_eq!(chars.next(), None, "14");
        chars.back();
        assert_eq!(chars.next(), Some('e'), "15");
        chars.back();
        chars.back();
        chars.back();
        assert_eq!(chars.next(), Some('c'), "16");
        assert_eq!(chars.next(), Some('d'), "17");
        assert_eq!(chars.next(), Some('e'), "18");
    }

    #[test]
    fn backtrack_peek_works() {
        let string = "abcde";
        let mut chars = Backtrackable::from(string.chars());
        assert_eq!(chars.next(), Some('a'), "1");
        assert_eq!(chars.peek(), Some('b'), "2");
        assert_eq!(chars.next(), Some('b'), "3");
        assert_eq!(chars.peek(), Some('c'), "4");
        assert_eq!(chars.next(), Some('c'), "5");
        chars.back();
        assert_eq!(chars.peek(), Some('c'), "6");
        assert_eq!(chars.next(), Some('c'), "7");
        assert_eq!(chars.peek(), Some('d'), "8");
        assert_eq!(chars.peek(), Some('d'), "9");
        assert_eq!(chars.next(), Some('d'), "10");
    }

    #[test]
    #[should_panic]
    fn backtrack_panics() {
        let string = "abcde";
        let mut chars = Backtrackable::from(string.chars());
        assert_eq!(chars.next(), Some('a'), "1");
        assert_eq!(chars.next(), Some('b'), "2");
        assert_eq!(chars.next(), Some('c'), "3");
        chars.back();
        chars.back();
        chars.back();
        chars.back();
        chars.back(); // One too many
    }

    #[test]
    fn cyclic_buffer_works() {
        let mut buffer = CyclicBuffer::<char>::new();
        assert_eq!(buffer.buffer, ['\0', '\0', '\0', '\0']);
        assert_eq!(buffer.count, 0);
        assert_eq!(buffer.get_back(0), None);
        assert_eq!(buffer.get_back(1), None);
        assert_eq!(buffer.get_back(2), None);
        assert_eq!(buffer.get_back(3), None);
        assert_eq!(buffer.get_back(4), None);
        assert_eq!(buffer.get_back(5), None);
        buffer.push('a');
        assert_eq!(buffer.buffer, ['a', '\0', '\0', '\0']);
        assert_eq!(buffer.count, 1);
        assert_eq!(buffer.get_back(0), Some('a'));
        assert_eq!(buffer.get_back(1), None);
        assert_eq!(buffer.get_back(2), None);
        buffer.push('b');
        assert_eq!(buffer.buffer, ['a', 'b', '\0', '\0']);
        assert_eq!(buffer.count, 2);
        assert_eq!(buffer.get_back(0), Some('b'));
        assert_eq!(buffer.get_back(1), Some('a'));
        assert_eq!(buffer.get_back(2), None);
        buffer.push('c');
        assert_eq!(buffer.buffer, ['a', 'b', 'c', '\0']);
        assert_eq!(buffer.count, 3);
        assert_eq!(buffer.get_back(0), Some('c'));
        assert_eq!(buffer.get_back(1), Some('b'));
        assert_eq!(buffer.get_back(2), Some('a'));
        assert_eq!(buffer.get_back(3), None);
        buffer.push('d');
        assert_eq!(buffer.buffer, ['a', 'b', 'c', 'd']);
        assert_eq!(buffer.count, 4);
        assert_eq!(buffer.get_back(0), Some('d'));
        assert_eq!(buffer.get_back(1), Some('c'));
        assert_eq!(buffer.get_back(2), Some('b'));
        assert_eq!(buffer.get_back(3), Some('a'));
        assert_eq!(buffer.get_back(4), None);
        buffer.push('e');
        assert_eq!(buffer.buffer, ['e', 'b', 'c', 'd']);
        assert_eq!(buffer.count, 5);
        assert_eq!(buffer.get_back(0), Some('e'));
        assert_eq!(buffer.get_back(1), Some('d'));
        assert_eq!(buffer.get_back(2), Some('c'));
        assert_eq!(buffer.get_back(3), Some('b'));
        assert_eq!(buffer.get_back(4), None);
        assert_eq!(buffer.get_back(5), None);
        buffer.push('f');
        assert_eq!(buffer.buffer, ['e', 'f', 'c', 'd']);
        assert_eq!(buffer.count, 6);
        assert_eq!(buffer.get_back(0), Some('f'));
        assert_eq!(buffer.get_back(1), Some('e'));
        assert_eq!(buffer.get_back(2), Some('d'));
        assert_eq!(buffer.get_back(3), Some('c'));
        assert_eq!(buffer.get_back(4), None);
        buffer.push('g');
        assert_eq!(buffer.buffer, ['e', 'f', 'g', 'd']);
        assert_eq!(buffer.count, 7);
        assert_eq!(buffer.get_back(0), Some('g'));
        assert_eq!(buffer.get_back(1), Some('f'));
        assert_eq!(buffer.get_back(2), Some('e'));
        assert_eq!(buffer.get_back(3), Some('d'));
        assert_eq!(buffer.get_back(4), None);
        buffer.push('h');
        assert_eq!(buffer.buffer, ['e', 'f', 'g', 'h']);
        assert_eq!(buffer.count, 8);
        assert_eq!(buffer.get_back(0), Some('h'));
        assert_eq!(buffer.get_back(1), Some('g'));
        assert_eq!(buffer.get_back(2), Some('f'));
        assert_eq!(buffer.get_back(3), Some('e'));
        assert_eq!(buffer.get_back(4), None);
        buffer.push('i');
        assert_eq!(buffer.buffer, ['i', 'f', 'g', 'h']);
        assert_eq!(buffer.count, 9);
        assert_eq!(buffer.get_back(0), Some('i'));
        assert_eq!(buffer.get_back(1), Some('h'));
        assert_eq!(buffer.get_back(2), Some('g'));
        assert_eq!(buffer.get_back(3), Some('f'));
        assert_eq!(buffer.get_back(4), None);
    }
}
