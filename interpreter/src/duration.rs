use chrono::Duration;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::number::complete::double;
use nom::IResult;

/// Parses a duration string into a [`Duration`]. Duration strings support the
/// following grammar:
///
/// DurationString -> Sign? Number Unit String?
/// Sign           -> '-'
/// Number         -> Digit+ ('.' Digit+)?
/// Digit          -> '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
/// Unit           -> 'h' | 'm' | 's' | 'ms' | 'us' | 'ns'
/// String         -> DurationString
///
/// # Examples
/// - `1h` parses as 1 hour
/// - `1.5h` parses as 1 hour and 30 minutes
/// - `1h30m` parses as 1 hour and 30 minutes
/// - `1h30m1s` parses as 1 hour, 30 minutes, and 1 second
/// - `1ms` parses as 1 millisecond
/// - `1.5ms` parses as 1 millisecond and 500 microseconds
/// - `1ns` parses as 1 nanosecond
/// - `1.5ns` parses as 1 nanosecond (sub-nanosecond durations not supported)
pub fn parse_duration(i: &str) -> IResult<&str, Duration> {
    let (i, neg) = opt(parse_negative)(i)?;
    if i == "0" {
        return Ok((i, Duration::zero()));
    }
    let (i, duration) = many1(parse_number_unit)(i)
        .map(|(i, d)| (i, d.iter().fold(Duration::zero(), |acc, next| acc + *next)))?;
    Ok((i, duration * if neg.is_some() { -1 } else { 1 }))
}

enum Unit {
    Nanosecond,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
}

impl Unit {
    fn nanos(&self) -> i64 {
        match self {
            Unit::Nanosecond => 1,
            Unit::Microsecond => 1_000,
            Unit::Millisecond => 1_000_000,
            Unit::Second => 1_000_000_000,
            Unit::Minute => 60 * 1_000_000_000,
            Unit::Hour => 60 * 60 * 1_000_000_000,
        }
    }
}

fn parse_number_unit(i: &str) -> IResult<&str, Duration> {
    let (i, num) = double(i)?;
    let (i, unit) = parse_unit(i)?;
    let duration = to_duration(num, unit);
    Ok((i, duration))
}

fn parse_negative(i: &str) -> IResult<&str, ()> {
    let (i, _): (&str, char) = char('-')(i)?;
    Ok((i, ()))
}

fn parse_unit(i: &str) -> IResult<&str, Unit> {
    alt((
        map(tag("ms"), |_| Unit::Millisecond),
        map(tag("us"), |_| Unit::Microsecond),
        map(tag("ns"), |_| Unit::Nanosecond),
        map(char('h'), |_| Unit::Hour),
        map(char('m'), |_| Unit::Minute),
        map(char('s'), |_| Unit::Second),
    ))(i)
}

fn to_duration(num: f64, unit: Unit) -> Duration {
    Duration::nanoseconds((num * unit.nanos() as f64).trunc() as i64)
}

#[cfg(test)]
mod tests {
    use crate::duration::parse_duration;
    use chrono::Duration;

    fn assert_duration(input: &str, expected: Duration) {
        let (_, duration) = parse_duration(input).unwrap();
        assert_eq!(duration, expected, "{}", input);
    }

    macro_rules! assert_durations {
        ($($str:expr => $duration:expr),*$(,)?) => {
            #[test]
            fn test_durations() {
                $(
                    assert_duration($str, $duration);
                )*
            }
        };
    }

    assert_durations! {
        "1s" => Duration::seconds(1),
        "-1s" => Duration::seconds(-1),
        "1.1s" => Duration::seconds(1) + Duration::milliseconds(100),
        "1.5m" => Duration::minutes(1) + Duration::seconds(30),
        "1m1s" => Duration::minutes(1) + Duration::seconds(1),
        "1h1m1s" => Duration::hours(1) + Duration::minutes(1) + Duration::seconds(1),
        "1ms" => Duration::milliseconds(1),
        "1us" => Duration::microseconds(1),
        "1ns" => Duration::nanoseconds(1),
        "1.1ns" => Duration::nanoseconds(1),
        "1.123us" => Duration::microseconds(1) + Duration::nanoseconds(123),
        "0s" => Duration::zero(),
        "0h0m0s" => Duration::zero(),
        "0h0m1s" => Duration::seconds(1),
        "0" => Duration::zero(),
        "-0" => Duration::zero(),
    }
}
