use super::sbnf::lexer::{Token};

use textwrap;

enum WhiteSpace {
    Newline,
    Space,
    Tab,
}

impl WhiteSpace {
    fn to_char(&self) -> char {
        match self {
            WhiteSpace::Newline => '\n',
            WhiteSpace::Space => ' ',
            WhiteSpace::Tab => '\t',
        }
    }
}

struct WhiteSpacePolicy {
    white_space: WhiteSpace,
    minimum: u32,
    maximum: u32,
}

struct SeparationPolicy {
    normal: WhiteSpacePolicy,
    pre_comment_separation: WhiteSpacePolicy,
    post_comment_separation: WhiteSpacePolicy,
}

struct Indentation {
    white_space: WhiteSpace,
    count: u32,
}

pub struct FormatterOptions {
    pub ensure_trailing_newline: bool,
    pub max_line_length: u32,
    pub element_separation: SeparationPolicy,
}

impl FormatterOptions {
    pub fn default() -> FormatterOptions {
        FormatterOptions {
            ensure_trailing_newline: true,
            max_line_length: 80,
        }
    }
}

pub fn format<'a>(options: FormatterOptions, tokens: &[Token<'a>]) -> String {
    let mut result = String::new();

    write_tokens(&options, tokens.iter().peek(), &mut result);

    if options.ensure_trailing_newline && result.chars().last().map_or(true, |c| c != '\n') {
        result.push('\n');
    }

    result
}

type TokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, Token<'a>>>;

fn write_tokens<'a>(options: &FormatterOptions, mut tokens: TokenIter<'a>, result: &mut String) {
    let mut num_elements = 0;

    loop {
        let sep = classify_separation(&mut tokens);

        if num_elements > 0 {
            write_separation(&options.element_separation, sep, result);
        } else {
            write_separation(&options.element_separation, sep, result);
        }

        match tokens.peek() {
            Some(Token::Identifier(name)) => {
                tokens.next();
                result.push_str(name);

                write_element(options, &mut tokens, result);
                num_elements += 1;
            },
            Some(Token::OpenSquareBracket) => {
                // TODO:
            },
            Some(Token::Comment(_)) | Some(Token::WhiteSpace(_)) | Some(Token::Newline) => panic!(),
            Some(_) => write_garbage(&mut tokens, result),
            None => break,
        }
    }
}

fn write_element(options: &FormatterOptions, tokens: &mut TokenIter<'a>, result: &mut String) {
    while let Some(token) = tokens.peek() {
        match token {
            Token::
        }
    }
}

fn write_nested_element()

fn write_garbage(tokens: &mut TokenIter<'a>, result: &mut String) {
    while let Some(token) = tokens.peek() {
        match token {
            Token::Identifier => break,
            _ => {
                tokens.next();

                result.push_str(&format!("{}", token));
            },
        }
    }
}

struct SeparationClassification<'a> {
    newlines: u32,
    spaces: u32,
    tabs: u32,
    comments: Vec<&'a str>,
}

fn classify_separation<'a>(tokens: &mut TokenIter<'a>) -> SeparationClassification<'a> {
    let mut newlines = 0;
    let mut spaces = 0;
    let mut tabs = 0;
    let mut comments = vec![];

    while let Some(token) = tokens.peek() {
        match token {
            Token::WhiteSpace(ws) => {
                for c in ws {
                    match c {
                        ' ' => {
                            spaces += 1;
                        },
                        '\t' => {
                            tabs += 1;
                        },
                        _ => panic!(),
                    }
                }
            },
            Token::Newline => {
                newlines += 1;
            },
            Token::Comment(c) => {
                comments.push(c);
            }
            _ => break,
        }

        tokens.next();
    }

    SeparationClassification {
        newlines,
        spaces,
        tabs,
        comments,
    }
}

fn write_separation<'a>(policy: &SeparationPolicy, indentation: Indentation, classification: SeparationClassification<'a>, result: &mut String) {
    if classification.comments.is_empty() {
        write_white_space(policy.normal, classification, result);
        return;
    }

    write_white_space(policy.pre_comment_separation, classification, result);

    write_white_space(policy.post_comment_separation, classification, result);
}

fn write_indentation(indentation: Indentation, result: &mut String) {
    let chr = indentation.to_char();
    for i in 0..indentation.count {
        result.push(chr);
    }
}

fn write_white_space<'a>(
    policy: &WhiteSpacePolicy,
    classification: SeparationClassification<'a>,
    result: &mut String,
) {
    let chr = policy.white_space.to_char();
    let count = match policy.white_space {
        WhiteSpace::Newline => classification.newlines,
        WhiteSpace::Space => classification.spaces,
        WhiteSpace::Tab => classification.tabs,
    };

    let count = std::cmp::min(std::cmp::max(count, policy.minimum), policy.maximum);
    for i in 0..count {
        result.push(chr);
    }
}

#[derive(Debug)]
struct CommentParagraph<'a> {
    prefix: &'a str,
    indentation: u32,
    content: String,
    completed: bool,
}

impl<'a> CommentParagraph<'a> {
    fn is_compatible_prefix(&self, prefix: &'a str) -> bool {
        if self.completed {
            return false;
        }

        if prefix.chars().count() != self.indentation as usize {
            return false;
        }

        for chr in prefix.chars() {
            if chr != ' ' {
                return false;
            }
        }

        true
    }
}

fn get_comment_line(line: &str) -> (&str, &str, bool) {
    let mut end = line.len();
    let mut has_content = false;

    for (index, chr) in line.char_indices() {
        if chr.is_alphanumeric() {
            end = index;
            has_content = true;
            break;
        } else if !chr.is_ascii_whitespace() {
            has_content = true;
        }
    }

    (&line[..end], &line[end..], has_content)
}

fn write_comment<'a>(options: &FormatterOptions, indentation: u32, iter: &mut TokenIter<'a>, result: &mut String) {
    let mut paragraphs: Vec<CommentParagraph> = Vec::new();
    let mut newline_count = 0;

    while let Some(current) = iter.current {
        match current {
            Token::Comment(string) => {
                newline_count = 0;

                let (prefix, line, has_content) = get_comment_line(&string[1..]);

                // Empty lines complete the current paragraph
                if !has_content {
                    if !paragraphs.is_empty() {
                        paragraphs.last_mut().unwrap().completed = true;
                    }
                    iter.next();
                    continue;
                }

                // Start a new paragraph
                if paragraphs.is_empty() || !paragraphs[paragraphs.len() - 1].is_compatible_prefix(prefix) {
                    paragraphs.push(CommentParagraph {
                        prefix: prefix,
                        indentation: prefix.chars().count() as u32,
                        content: String::new(),
                        completed: false,
                    });
                }

                let paragraph = paragraphs.last_mut().unwrap();

                // Ignore the # at the front of the token
                if !paragraph.content.is_empty() {
                    paragraph.content.push(' ');
                }
                paragraph.content.push_str(line);
            },
            Token::Newline => {
                newline_count += 1;
                if newline_count == 2 {
                    iter.next();
                    break;
                }
            },
            Token::Whitespace(_) => {},
            _ => break,
        }

        iter.next();
    }

    let mut last_did_complete = false;
    for mut paragraph in paragraphs {
        if last_did_complete {
            write_indentation(indentation, result);
            result.push_str("#\n");
        }
        last_did_complete = paragraph.completed;

        if paragraph.prefix.chars().next() == Some(' ') {
            paragraph.prefix = &paragraph.prefix[1..];
            paragraph.indentation -= 1;
        }

        let wrap_width = options.max_line_length - (indentation + "# ".len() as u32 + paragraph.indentation);

        for (index, line) in textwrap::wrap_iter(&paragraph.content, wrap_width as usize).enumerate() {
            write_indentation(indentation, result);
            result.push_str("# ");

            if index == 0 {
                result.push_str(paragraph.prefix);
            } else {
                write_indentation(paragraph.indentation, result);
            }

            result.push_str(&*line);
            result.push('\n');
        }
    }

    if newline_count == 2 {
        result.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use crate::sbnf::lexer::lex;
    use super::*;

    fn fmt(source: &str) -> String {
        let grammar = lex(source);
        println!("{:?}", grammar.tokens);
        format(FormatterOptions::default(), &grammar.tokens)
    }

    #[test]
    fn format_comment() {
        assert!(fmt("") == "\n");
        assert!(fmt("\n") == "\n");
        assert!(fmt("\n\n") == "\n");
        assert!(fmt("\n\n\n") == "\n");
        assert!(fmt("#foo") == "# foo\n");
        assert!(fmt("# foo\n") == "# foo\n");
        assert!(fmt(" # foo\n") == "# foo\n");
        assert!(fmt(" # a long line that should get wrapped around due to the default max line length being 80 characters.") == "# a long line that should get wrapped around due to the default max line length\n# being 80 characters.\n");
        assert!(fmt("#foo\n#bar\n") == "# foo bar\n");
        assert!(fmt("#foo\n#\n#bar\n") == "# foo\n#\n# bar\n");
        assert!(fmt("#foo\n#\n#\n#bar\n") == "# foo\n#\n# bar\n");
        assert!(fmt("# foo\n\n# bar\n") == "# foo\n\n# bar\n");
        assert!(fmt("# Here's some examples:\n\
# * foo bar\n\
# * A longer example that should cause the line to wrap at \
80 characters onto the next line.\n\
#     Further indented paragraph here\n")
                == "# Here's some examples:\n\
# * foo bar\n\
# * A longer example that should cause the line to wrap at \
80 characters onto\n\
#   the next line.\n\
#     Further indented paragraph here\n");
        // Invalid
        assert!(fmt(";") == ";\n");
    }
}
