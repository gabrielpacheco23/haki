use crate::expr::LispExp;
use std::iter::Peekable;

pub fn tokenize(expr: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut current_token = String::new();
    let mut inside_string = false;
    let mut inside_comment = false;
    let mut chars = expr.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\n' && inside_comment {
            inside_comment = false;
            continue;
        }
        if inside_comment {
            continue;
        }

        if inside_string {
            if c == '\\' {
                // --- MÁGICA DOS ESCAPES ---
                // Se achou '\', pega o próximo caractere
                if let Some(escaped) = chars.next() {
                    match escaped {
                        'n' => current_token.push('\n'),
                        't' => current_token.push('\t'),
                        'r' => current_token.push('\r'),
                        '"' => current_token.push('"'),
                        '\\' => current_token.push('\\'),
                        // Se for um escape inválido/desconhecido, mantemos os dois caracteres originais
                        _ => {
                            current_token.push('\\');
                            current_token.push(escaped);
                        }
                    }
                }
                // (Se a string acabar logo após o '\', ignoramos, a aspa de fecho vai falhar de qualquer modo)
            } else if c == '"' {
                inside_string = false;
                current_token.push(c); // Inclui aspa de fechamento
                tokens.push(current_token.clone());
                current_token.clear();
            } else {
                current_token.push(c);
            }
        } else {
            match c {
                ';' => {
                    inside_comment = true;
                    if !current_token.is_empty() {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                }
                '(' | ')' | '\'' | '`' | ',' | ']' | '[' | '{' | '}' | 'λ' => {
                    if !current_token.is_empty() {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                    tokens.push(c.to_string());
                }
                '"' => {
                    if !current_token.is_empty() {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                    inside_string = true;
                    current_token.push(c); // Inclui aspa de abertura
                }
                c if c.is_whitespace() => {
                    if !current_token.is_empty() {
                        tokens.push(current_token.clone());
                        current_token.clear();
                    }
                }
                _ => current_token.push(c),
            }
        }
    }

    if !current_token.is_empty() {
        tokens.push(current_token);
    }

    tokens
}

pub fn read_from_tokens<I>(mut tokens: &mut Peekable<I>) -> Result<LispExp, &'static str>
where
    I: Iterator<Item = String>,
{
    let token = tokens.next().ok_or("Unexpected EOF (End of File)")?;

    match token.as_str() {
        "(" => {
            let mut list_vec = vec![];

            while let Some(next_token) = tokens.peek() {
                if next_token == ")" {
                    break;
                }
                let exp = read_from_tokens(&mut tokens)?;
                list_vec.push(exp);
            }

            if tokens.next().is_none() {
                return Err("Expected ')' but found EOF");
            }

            Ok(LispExp::List(list_vec))
        }
        "[" => {
            let mut list = vec![LispExp::Symbol("vector".to_string())];
            while let Some(t) = tokens.peek() {
                if *t == "]" {
                    tokens.next();
                    return Ok(LispExp::List(list));
                }
                list.push(read_from_tokens(tokens)?);
            }
            Err("Missing closing brackets ']'")
        }

        "{" => {
            let mut list = vec![LispExp::Symbol("hash".to_string())];
            while let Some(t) = tokens.peek() {
                if *t == "}" {
                    tokens.next();
                    return Ok(LispExp::List(list));
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
            Ok(LispExp::List(vec![
                LispExp::Symbol("quote".to_string()),
                exp,
            ]))
        }
        "`" => {
            let exp = read_from_tokens(tokens)?;
            Ok(LispExp::List(vec![
                LispExp::Symbol("quasiquote".to_string()),
                exp,
            ]))
        }
        "," => {
            let exp = read_from_tokens(tokens)?;
            Ok(LispExp::List(vec![
                LispExp::Symbol("unquote".to_string()),
                exp,
            ]))
        }
        "#t" => Ok(LispExp::Bool(true)),
        "#f" => Ok(LispExp::Bool(false)),
        s if s.starts_with('"') && s.ends_with('"') && token.len() >= 2 => {
            // let content = s.trim_matches('"').to_string();
            let inner_str = &s[1..s.len() - 1];
            Ok(LispExp::Str(inner_str.to_string()))
        }
        _ => Ok(atom(&token)),
    }
}

fn atom(token: &str) -> LispExp {
    if let Ok(f) = token.parse::<f64>() {
        return LispExp::Number(f);
    }
    LispExp::Symbol(token.to_string())
}
