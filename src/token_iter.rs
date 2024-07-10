use crate::lex::{Token, TokenRef};

pub struct TokenIter {
    tokens: Vec<TokenRef>,
    index: usize,
}

impl From<Vec<TokenRef>> for TokenIter {
    fn from(tokens: Vec<TokenRef>) -> Self {
        Self { tokens, index: 0 }
    }
}

impl TokenIter {
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> &Token {
        let Some(token_ref) = self.tokens.get(self.index) else {
            return &Token::Eof;
        };
        self.index += 1;
        &token_ref.token
    }

    pub fn peek(&mut self) -> &Token {
        let Some(token_ref) = self.tokens.get(self.index) else {
            return &Token::Eof;
        };
        &token_ref.token
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn set_index(&mut self, index: usize) -> &Token {
        //TODO: Handle underflow
        let token = match self.tokens.get(self.index - 1) {
            Some(token_ref) => &token_ref.token,
            None => &Token::Eof,
        };
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

    pub fn line(&self) -> usize {
        self.tokens
            .get(self.index - 1)
            .map_or(0, |token_ref| token_ref.line)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::Literal;

    #[test]
    fn normal_iter() {
        let tokens = vec![
            TokenRef {
                token: Token::Literal(Literal::Number(1.0)),
                line: 0,
            },
            TokenRef {
                token: Token::Literal(Literal::Number(2.0)),
                line: 0,
            },
            TokenRef {
                token: Token::Literal(Literal::Number(3.0)),
                line: 0,
            },
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
            TokenRef {
                token: Token::Literal(Literal::Number(1.0)),
                line: 0,
            },
            TokenRef {
                token: Token::Literal(Literal::Number(2.0)),
                line: 0,
            },
            TokenRef {
                token: Token::Literal(Literal::Number(3.0)),
                line: 0,
            },
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

    //TODO: Add test with line numbers
}
