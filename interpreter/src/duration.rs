use chrono::Duration;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::{map, opt};
use nom::multi::many1;
use nom::number::complete::double;
use nom::IResult;

static NANOS_PER_SEC: f64 = 1e9;
static NANOS_PER_MILLI: f64 = 1e6;
static NANOS_PER_MICRO: f64 = 1e3;

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
    if i == "0" {
        return Ok(("", Duration::zero()));
    }
    let (i, neg) = opt(parse_negative)(i)?;
    let (_, duration): (&str, Duration) = many1(parse_number_unit)(i)
        .map(|(i, d)| (i, d.iter().fold(Duration::zero(), |acc, next| acc + *next)))?;
    Ok(("", duration * if neg.is_some() { -1 } else { 1 }))
}

enum Unit {
    Hour,
    Minute,
    Second,
    Millisecond,
    Microsecond,
    Nanosecond,
}

fn parse_number_unit(i: &str) -> IResult<&str, Duration> {
    let (i, num) = double(i)?;
    let (i, unit) = parse_unit(i)?;
    let duration = match unit {
        Unit::Hour => to_duration(num, 3600.0 * NANOS_PER_SEC),
        Unit::Minute => to_duration(num, 60.0 * NANOS_PER_SEC),
        Unit::Second => to_duration(num, 1.0 * NANOS_PER_SEC),
        Unit::Millisecond => to_duration(num, NANOS_PER_MILLI),
        Unit::Microsecond => to_duration(num, NANOS_PER_MICRO),
        Unit::Nanosecond => Duration::nanoseconds(num.round() as i64),
    };
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

/// Converts a number and a unit into a Duration.
///
/// This function is used to convert a floating-point number and a unit into a Duration.
/// The number represents the quantity of the unit, and the unit is given in nanoseconds.
///
/// # Arguments
///
/// * `num` - The number of units as a floating-point number. This can include a fractional part.
/// * `unit_in_ns` - The duration of one unit in nanoseconds. This is used to convert the number into a Duration.
///
/// # Returns
///
/// This function returns a Duration that represents the given number of units.
///
/// # Examples
///
/// ```skip
/// let duration = to_duration(1.5, 1e9); // Returns a Duration of 1.5 seconds
/// ```
fn to_duration(num: f64, unit_in_ns: f64) -> Duration {
    let whole = (num.trunc() * unit_in_ns) as i64;
    let fractional = (num.fract() * unit_in_ns) as i64; // Now directly multiply the fractional part with unit_in_ns
    Duration::nanoseconds(whole + fractional)
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
    }
}
