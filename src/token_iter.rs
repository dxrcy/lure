use crate::lex::Token;

pub struct TokenIter {
    tokens: Vec<Token>,
    index: usize,
}

impl From<Vec<Token>> for TokenIter {
    fn from(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }
}

impl TokenIter {
    pub fn next(&mut self) -> &Token {
        let Some(token) = self.tokens.get(self.index) else {
            return &Token::Eof;
        };
        self.index += 1;
        token
    }

    pub fn peek(&mut self) -> &Token {
        let Some(token) = self.tokens.get(self.index) else {
            return &Token::Eof;
        };
        token
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn set_index(&mut self, index: usize) -> &Token {
        //TODO: Handle underflow
        let token = self.tokens.get(self.index - 1).unwrap_or(&Token::Eof);
        self.index = index;
        token
    }

    pub fn reverse(&mut self, offset: usize) -> bool {
        if offset > self.index {
            return false;
        }
        self.index -= offset;
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::Literal;

    #[test]
    fn normal_iter() {
        let tokens = vec![
            Token::Literal(Literal::Number(1.0)),
            Token::Literal(Literal::Number(2.0)),
            Token::Literal(Literal::Number(3.0)),
        ];
        let mut iter = TokenIter::from(tokens);
        assert_eq!(iter.index, 0);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(1.0)));
        assert_eq!(iter.index, 1);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(2.0)));
        assert_eq!(iter.index, 2);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(3.0)));
        assert_eq!(iter.index, 3);
        assert_eq!(iter.next(), &Token::Eof);
        assert_eq!(iter.index, 3);
        assert_eq!(iter.next(), &Token::Eof);
        assert_eq!(iter.index, 3);
    }

    #[test]
    fn save_index() {
        let tokens = vec![
            Token::Literal(Literal::Number(1.0)),
            Token::Literal(Literal::Number(2.0)),
            Token::Literal(Literal::Number(3.0)),
        ];
        let mut iter = TokenIter::from(tokens);
        assert_eq!(iter.index, 0);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(1.0)));
        assert_eq!(iter.index, 1);
        let index = iter.get_index();
        assert_eq!(index, 1);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(2.0)));
        assert_eq!(iter.index, 2);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(3.0)));
        assert_eq!(iter.index, 3);
        assert_eq!(iter.set_index(index), &Token::Literal(Literal::Number(3.0)));
        assert_eq!(iter.index, 1);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(2.0)));
        assert_eq!(iter.index, 2);
        assert_eq!(iter.next(), &Token::Literal(Literal::Number(3.0)));
        assert_eq!(iter.index, 3);
        assert_eq!(iter.next(), &Token::Eof);
        assert_eq!(iter.index, 3);
        assert_eq!(iter.next(), &Token::Eof);
        assert_eq!(iter.index, 3);
    }
}
