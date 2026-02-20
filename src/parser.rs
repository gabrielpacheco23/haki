use crate::expr::LispExp;
use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub text: String,
    pub line: usize,
}

pub fn tokenize(expr: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut inside_string = false;
    let mut inside_comment = false;
    let mut chars = expr.chars().peekable();

    let mut current_line = 1;

    while let Some(c) = chars.next() {
        if c == '\n' {
            current_line += 1;
            if inside_comment {
                inside_comment = false;
                continue;
            }
        }

        if inside_comment {
            continue;
        }

        if inside_string {
            if c == '\\' {
                if let Some(escaped) = chars.next() {
                    if escaped == '\n' {
                        current_line += 1;
                    }
                    match escaped {
                        'n' => current_token.push('\n'),
                        't' => current_token.push('\t'),
                        'r' => current_token.push('\r'),
                        '"' => current_token.push('"'),
                        '\\' => current_token.push('\\'),
                        _ => {
                            current_token.push('\\');
                            current_token.push(escaped);
                        }
                    }
                }
            } else if c == '"' {
                inside_string = false;
                current_token.push(c);
                tokens.push(Token {
                    text: current_token.clone(),
                    line: current_line,
                });
                current_token.clear();
            } else {
                current_token.push(c);
            }
        } else {
            match c {
                ';' => {
                    inside_comment = true;
                    if !current_token.is_empty() {
                        tokens.push(Token {
                            text: current_token.clone(),
                            line: current_line,
                        });
                        current_token.clear();
                    }
                }
                '(' | ')' | '\'' | '`' | ',' | ']' | '[' | '{' | '}' | 'λ' => {
                    if !current_token.is_empty() {
                        tokens.push(Token {
                            text: current_token.clone(),
                            line: current_line,
                        });
                        current_token.clear();
                    }
                    tokens.push(Token {
                        text: c.to_string(),
                        line: current_line,
                    });
                }
                '"' => {
                    if !current_token.is_empty() {
                        tokens.push(Token {
                            text: current_token.clone(),
                            line: current_line,
                        });
                        current_token.clear();
                    }
                    inside_string = true;
                    current_token.push(c);
                }
                c if c.is_whitespace() => {
                    if !current_token.is_empty() {
                        tokens.push(Token {
                            text: current_token.clone(),
                            line: current_line,
                        });
                        current_token.clear();
                    }
                }
                _ => current_token.push(c),
            }
        }
    }

    if !current_token.is_empty() {
        tokens.push(Token {
            text: current_token,
            line: current_line,
        });
    }

    tokens
}

pub fn read_from_tokens<I>(tokens: &mut Peekable<I>) -> Result<LispExp, &'static str>
where
    I: Iterator<Item = Token>,
{
    let token = tokens.next().ok_or("Unexpected EOF (End of File)")?;

    let text = token.text;
    let line = token.line;

    match text.as_str() {
        "(" => {
            let mut list_vec = vec![];

            while let Some(next_token) = tokens.peek() {
                if next_token.text == ")" {
                    break;
                }
                let exp = read_from_tokens(tokens)?;
                list_vec.push(exp);
            }

            if tokens.next().is_none() {
                return Err("Expected ')' but found EOF");
            }

            Ok(LispExp::List(list_vec, line))
        }
        "[" => {
            let mut list = vec![LispExp::Symbol("vector".to_string(), line)];
            while let Some(t) = tokens.peek() {
                if t.text == "]" {
                    tokens.next();
                    return Ok(LispExp::List(list, line));
                }
                list.push(read_from_tokens(tokens)?);
            }
            Err("Missing closing brackets ']'")
        }

        "{" => {
            let mut list = vec![LispExp::Symbol("hash".to_string(), line)];
            while let Some(t) = tokens.peek() {
                if t.text == "}" {
                    tokens.next();
                    return Ok(LispExp::List(list, line));
                }
                list.push(read_from_tokens(tokens)?);
            }
            Err("Missing closing brackets '}'")
        }

        ")" => Err("Unexpected ')'"),
        "]" => Err("Unexpected ']'"),
        "}" => Err("Unexpected '}'"),
        "'" => {
            let exp = read_from_tokens(tokens)?;
            Ok(LispExp::List(
                vec![LispExp::Symbol("quote".to_string(), line), exp],
                line,
            ))
        }
        "`" => {
            let exp = read_from_tokens(tokens)?;
            Ok(LispExp::List(
                vec![LispExp::Symbol("quasiquote".to_string(), line), exp],
                line,
            ))
        }
        "," => {
            let exp = read_from_tokens(tokens)?;
            Ok(LispExp::List(
                vec![LispExp::Symbol("unquote".to_string(), line), exp],
                line,
            ))
        }
        "#t" => Ok(LispExp::Bool(true)),
        "#f" => Ok(LispExp::Bool(false)),
        s if s.starts_with('"') && s.ends_with('"') && text.len() >= 2 => {
            let inner_str = &s[1..s.len() - 1];
            Ok(LispExp::Str(inner_str.to_string()))
        }
        _ => Ok(atom(&text, line)),
    }
}

fn atom(token: &str, line: usize) -> LispExp {
    if let Ok(f) = token.parse::<f64>() {
        return LispExp::Number(f);
    }
    LispExp::Symbol(token.to_string(), line)
}
