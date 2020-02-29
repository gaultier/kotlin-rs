use crate::lex::{Lexer, Token, TokenKind};
use std::option::Option;

type Precedence = u8;
const PREC_NONE: Precedence = 0;
const PREC_ASSIGNEMENT: Precedence = 1; // =
const PREC_OR: Precedence = 2;
const PREC_AND: Precedence = 3;
const PREC_EQUALITY: Precedence = 4; // == !=
const PREC_COMPARISON: Precedence = 5; // < > <= >=
const PREC_TERM: Precedence = 6; // + -
const PREC_FACTOR: Precedence = 7; // * /
const PREC_UNARY: Precedence = 8; // ! -
const PREC_CALL: Precedence = 9; // . ()
const PREC_PRIMARY: Precedence = 10;

#[derive(Debug)]
struct Parser<'a> {
    previous: Option<Token>,
    current: Option<Token>,
    lexer: Lexer<'a>,
}

type ParseFn = fn(&mut Parser) -> ();

struct ParseRule {
    precedence: Precedence,
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
}

fn binary(parser: &mut Parser) {
    println!(
        "binary: prev={:?} cur={:?}",
        parser.previous, parser.current
    );
    let previous_type = parser.previous.as_ref().unwrap().kind.clone();
    dbg!(&previous_type);
    let previous_precedence = RULES[usize::from(&previous_type)].precedence;
    parser.precedence(previous_precedence + 1);

    match previous_type {
        TokenKind::Plus => {
            dbg!("plus");
        }
        _ => unimplemented!(),
    }
}

fn number(parser: &mut Parser) {
    println!(
        "number: prev={:?} cur={:?}",
        parser.previous, parser.current
    );
}

const RULES: [ParseRule; 144] = [
    // Plus
    ParseRule {
        precedence: PREC_TERM,
        infix: Some(binary),
        prefix: None,
    },
    // PlusPlus
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Minus
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // MinusMinus
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Slash
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Star
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Equal
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // EqualEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // EqualEqualEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Comma
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // At
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Dollar
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // LeftParen
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // RightParen
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // LeftSquareBracket
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // RightSquareBracket
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // LeftCurlyBracket
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // RightCurlyBracket
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Dot
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Ampersand
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // AmpersandAmpersand
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Pipe
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // PipePipe
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Bang
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // BangEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // BangEqualEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Colon
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // ColonColon
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Semicolon
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // SemicolonSemicolon
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // PlusEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // MinusEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // StarEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // SlashEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Percent
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // PercentEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Smaller
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // SmallerEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Greater
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // GreaterEqual
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Arrow
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // FatArrow
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    // Int
    ParseRule {
        precedence: PREC_NONE,
        infix: None,
        prefix: Some(number),
    },
    // Long
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
    ParseRule {
        precedence: PREC_FACTOR,
        infix: None,
        prefix: None,
    },
];

impl<'a> Parser<'a> {
    fn expression(&mut self) {
        self.precedence(PREC_ASSIGNEMENT)
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();

        loop {
            let cur = self.lexer.lex();
            if let Ok(cur) = cur {
                self.current = Some(cur);
                return;
            } else {
                println!("Lexer error: {:?}", self.current);
                // TODO: error
            }
        }
    }

    fn is_at_end(&self) -> bool {
        self.current
            .as_ref()
            .map(|cur| cur.is_eof())
            .unwrap_or(true)
    }

    fn precedence(&mut self, precedence: Precedence) {
        dbg!(&self.current);
        self.advance();
        dbg!(&self.current);

        let previous_kind = &self.previous.as_ref().unwrap().kind;
        let previous_index = usize::from(previous_kind);
        dbg!(previous_kind, previous_index);
        let prefix_rule = RULES[previous_index].prefix.unwrap();
        prefix_rule(self);

        let current_kind = &self.current.as_ref().unwrap().kind;
        let current_index = usize::from(current_kind);
        let mut current_precedence = RULES[current_index].precedence;
        dbg!(&current_precedence, &precedence);

        while !self.is_at_end() && precedence <= current_precedence {
            self.advance();

            let previous_kind = &self.previous.as_ref().unwrap().kind;
            let previous_index = usize::from(previous_kind);
            let infix_rule = RULES[previous_index].infix.unwrap();
            infix_rule(self);

            let current_kind = &self.current.as_ref().unwrap().kind;
            let current_index = usize::from(current_kind);
            current_precedence = RULES[current_index].precedence;
            dbg!(current_precedence, precedence);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t() {
        let s = "1 + 2";
        let mut parser = Parser {
            previous: None,
            current: None,
            lexer: Lexer::new(&s),
        };
        parser.advance();
        parser.expression();
        assert!(false);
    }
}
