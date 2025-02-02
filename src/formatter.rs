use crate::sbnf::tokenizer::{TokenType, Token, Tokenizer};

enum LineBreakPolicy {
    Always,
    Never,
}

enum WhiteSpacePolicy {

}

struct Construct<'a> {
    kind: ConstructKind,
    children: Vec<ConstructOrToken<'a>>,
}

enum ConstructOrToken<'a> {
    Construct(Construct<'a>),
    Token(Token<'a>),
}

impl<'a> Construct<'a> {
    fn bare(children: Vec<ConstructOrToken<'a>>) -> Self {
        ConstructOrToken::Construct(Construct {
            kind: ConstructKind::Bare,
            children,
        })
    }
}

enum ConstructKind {
    Bare,
    CommentParagraph,
    Variable,
    Rule,
    Parameters,
    Options,
}

pub struct Config {
    line_length_limit: Option<NonZeroU16>,
}

pub fn format(source: &str, config: &Config) -> String {
    let mut tokenizer = Tokenizer::new(source);

    let mut constructs = vec![];
    while let Some(construct) = parse_construct(&mut tokenizer) {
        constructs.push(construct);
    }
}

fn skip_non_semantic(tokenizer: &mut Tokenizer, children: &mut Vec<ConstructOrToken>) -> Option<Token> {
    while let Some(token) = tokenizer.next() {
        match token.type_ {
            TokenType::Whitespace | TokenType::LineComment | TokenType::Error => {
                children.push(ConstructOrToken::Token(token));
            },
            _ => {
                return Some(Token);
            }
        }
    }

    None
}

fn parse_construct(tokenizer: &mut Tokenizer) -> Option<ConstructOrToken> {
    let mut children = vec![];

    let Some(token) = skip_non_semantic() else {
        return if children.is_empty() {
            None
        } else {
            Some(ConstructOrToken::bare(children))
        };
    };

    match token.type_ {
        TokenType::OpenParenthesis => {
            let mut c = vec![];
            while let Some(child) = parse_construct(tokenizer) {
                c.push(child);
            }
        }
        TokenType::OpenSquareBracket => {

        }
    }
}
