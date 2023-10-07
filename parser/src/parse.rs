use std::num::ParseIntError;

/// Error type of [unescape](unescape).
#[derive(Debug, PartialEq)]
pub enum ParseError {
    // #[error("invalid escape {escape} at {index} in {string}")]
    InvalidEscape {
        escape: String,
        index: usize,
        string: String,
    },
    // #[error("\\u could not be parsed at {index} in {string}: {source}")]
    InvalidUnicode {
        // #[source]
        source: ParseUnicodeError,
        index: usize,
        string: String,
    },
}

/// Source error type of [ParseError::InvalidUnicode](ParseError::InvalidUnicode).
#[derive(Debug, PartialEq)]
pub enum ParseUnicodeError {
    // #[error("could not parse {string} as u32 hex: {source}")]
    ParseHexFailed {
        // #[source]
        source: ParseIntError,
        string: String,
    },
    ParseOctFailed {
        // #[source]
        source: ParseIntError,
        string: String,
    },
    // #[error("could not parse {value} as a unicode char")]
    ParseUnicodeFailed { value: u32 },
}

/// Parse the provided quoted string.
/// This function was adopted from [snailquote](https://docs.rs/snailquote/latest/snailquote/).
///
/// # Details
///
/// Parses a single or double quoted string and interprets escape sequences such as
/// '\n', '\r', '\'', etc.
///
/// Supports raw strings prefixed with `r` or `R` in which case all escape sequences are ignored.///
///
/// The full set of supported escapes between quotes may be found below:
///
/// | Escape     | Code       | Description                              |
/// |------------|------------|------------------------------------------|
/// | \a         | 0x07       | Bell                                     |
/// | \b         | 0x08       | Backspace                                |
/// | \v         | 0x0B       | Vertical tab                             |
/// | \f         | 0x0C       | Form feed                                |
/// | \n         | 0x0A       | Newline                                  |
/// | \r         | 0x0D       | Carriage return                          |
/// | \t         | 0x09       | Tab                                      |
/// | \\         | 0x5C       | Backslash                                |
/// | \?         | 0x??       | Question mark                            |
/// | \"         | 0x22       | Double quote                             |
/// | \'         | 0x27       | Single quote                             |
/// | \`         | 0x??       | Backtick                                 |
/// | \xDD       | 0xDD       | Unicode character with hex code DD       |
/// | \uDDDD     | 0xDDDD     | Unicode character with hex code DDDD     |
/// | \UDDDDDDDD | 0xDDDDDDDD | Unicode character with hex code DDDDDDDD |
/// | \DDD       | 0DDD       | Unicode character with octal code DDD    |
///
/// # Errors
///
/// The returned result can display a human readable error if the string cannot be parsed as a
/// valid quoted string.
///

pub fn parse_string(s: &str) -> Result<String, ParseError> {
    let mut in_single_quotes = false;
    let mut in_double_quotes = false;
    let mut in_raw_string = false;

    let mut chars = s.chars().enumerate();
    let mut res = String::with_capacity(s.len());

    while let Some((idx, c)) = chars.next() {
        let in_quotes = in_single_quotes || in_double_quotes;

        if !in_quotes && (c == 'r' || c == 'R') {
            in_raw_string = true;
            continue;
        } else if c == '\\' && !in_raw_string {
            if in_quotes {
                match chars.next() {
                    None => {
                        return Err(ParseError::InvalidEscape {
                            escape: format!("{}", c),
                            index: idx,
                            string: String::from(s),
                        });
                    }
                    Some((idx, c2)) => {
                        let s = match c2 {
                            'a' => String::from("\u{07}"),
                            'b' => String::from("\u{08}"),
                            'v' => String::from("\u{0B}"),
                            'f' => String::from("\u{0C}"),
                            'n' => String::from("\n"),
                            'r' => String::from("\r"),
                            't' => String::from("\t"),
                            '\\' => String::from("\\"),
                            '?' => String::from("?"),
                            '\'' => {
                                if in_double_quotes {
                                    String::from("\\'")
                                } else {
                                    String::from("'")
                                }
                            },
                            '"' => {
                                if in_single_quotes {
                                    String::from("\\\"")
                                } else {
                                    String::from("\"")
                                }
                            },
                            '`' => String::from("`"),
                            'x' => parse_unicode_hex(2, &mut chars).map_err(|x| {
                                ParseError::InvalidUnicode {
                                    source: x,
                                    index: idx,
                                    string: String::from(s),
                                }
                            }).unwrap(),
                            'u' => parse_unicode_hex(4, &mut chars).map_err(|x| {
                                ParseError::InvalidUnicode {
                                    source: x,
                                    index: idx,
                                    string: String::from(s),
                                }
                            }).unwrap(),
                            'U' => parse_unicode_hex(8, &mut chars).map_err(|x| {
                                ParseError::InvalidUnicode {
                                    source: x,
                                    index: idx,
                                    string: String::from(s),
                                }
                            }).unwrap(),
                            x if ('0'..='3').contains(&x) => parse_unicode_oct(&x, &mut chars).map_err(|x| {
                                ParseError::InvalidUnicode {
                                    source: x,
                                    index: idx,
                                    string: String::from(s),
                                }
                            }).unwrap(),
                            _ => {
                                return Err(ParseError::InvalidEscape {
                                    escape: format!("{}{}", c, c2),
                                    index: idx,
                                    string: String::from(s),
                                });
                            }
                        };
                        res.push_str(s.as_str());
                        continue;
                    }
                };
            } else {
                return Err(ParseError::InvalidEscape {
                    escape: format!("{}", c),
                    index: idx,
                    string: String::from(s),
                });
            }
        }
        else if c == '\'' {
            if in_double_quotes {
                res.push(c);
                continue
            }

            in_single_quotes = !in_single_quotes;
            continue;
        } else if c == '"' {
            if in_single_quotes {
                res.push(c);
                continue
            }

            in_double_quotes = !in_double_quotes;
            continue;
        }

        res.push(c);
    }

    Ok(res)
}

fn parse_unicode_hex<I>(length: usize, chars: &mut I) -> Result<String, ParseUnicodeError>
    where
        I: Iterator<Item = (usize, char)>,
{
    let unicode_seq: String = chars
        .take(length)
        .map(|(_, c)| c)
        .collect();

    u32::from_str_radix(&unicode_seq, 16)
        .map_err(|e| ParseUnicodeError::ParseHexFailed {
            source: e,
            string: unicode_seq,
        })
        .and_then(|u| {
            char::from_u32(u)
                .and_then(|c| Some(c.to_string()))
                .ok_or_else(|| ParseUnicodeError::ParseUnicodeFailed { value: u })
        })
}

fn parse_unicode_oct<I>(first_char: &char, chars: &mut I) -> Result<String, ParseUnicodeError>
    where
        I: Iterator<Item = (usize, char)>,
{
    let mut unicode_seq: String = String::with_capacity(3);
    unicode_seq.push(*first_char);
    chars.take(2).for_each(|(_, c)| unicode_seq.push(c));

    u32::from_str_radix(&unicode_seq, 8)
        .map_err(|e| ParseUnicodeError::ParseOctFailed {
            source: e,
            string: unicode_seq,
        })
        .and_then(|u| {
            if u <= 255 {
                char::from_u32(u)
                    .and_then(|c| Some(c.to_string()))
                    .ok_or_else(|| ParseUnicodeError::ParseUnicodeFailed { value: u })
            } else {
                Err(ParseUnicodeError::ParseUnicodeFailed { value: u })
            }
        })
}
