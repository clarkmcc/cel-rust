use crate::objects::Map;
use crate::Value;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1};
use nom::combinator::{map_res, opt, recognize};
use nom::sequence::{delimited, preceded};
use nom::IResult;
use std::str::FromStr;
use std::sync::Arc;

pub fn parse_value(input: &str) -> Result<Value, ()> {
    let (_, v) = _parse_value(input).map_err(|err| {
        eprintln!("Error parsing value: {:?}", err);
        ()
    })?;
    Ok(v)
}

fn _parse_value(input: &str) -> IResult<&str, Value> {
    alt((
        parse_none,
        parse_bytes,
        parse_double,
        parse_int,
        parse_uint,
        parse_string,
        parse_bool,
        parse_list,
        parse_map,
    ))(input)
}

fn parse_int(input: &str) -> IResult<&str, Value> {
    let (i, _) = tag("IntType(source=")(input)?;
    let (i, v) = map_res(recognize(preceded(opt(char('-')), digit1)), |s| {
        i64::from_str(s)
    })(i)?;
    Ok((i, Value::Int(v)))
}

fn parse_uint(input: &str) -> IResult<&str, Value> {
    let (i, _) = tag("UintType(source=")(input)?;
    let (i, v) = map_res(recognize(digit1), u64::from_str)(i)?;
    Ok((i, Value::UInt(v)))
}

fn parse_double(input: &str) -> IResult<&str, Value> {
    let (i, _) = tag("DoubleType(source=")(input)?;
    let (i, v) = map_res(recognize(preceded(opt(char('-')), digit1)), |s| {
        f64::from_str(s)
    })(i)?;
    Ok((i, Value::Float(v)))
}

fn parse_string(i: &str) -> IResult<&str, Value> {
    let (i, _) = tag("StringType(source=")(i)?;
    let (i, value_str) = delimited(
        alt((char('"'), char('\''))),
        alt((tag("!"), nom::character::complete::alphanumeric0)),
        alt((char('"'), char('\''))),
    )(i)?;
    Ok((i, Value::String(Arc::new(value_str.to_string()))))
}

fn parse_bytes(i: &str) -> IResult<&str, Value> {
    let (i, _) = tag("BytesType(source=b")(i)?;
    let (i, value_str) = delimited(
        char('\''),
        nom::character::complete::alphanumeric0,
        char('\''),
    )(i)?;
    Ok((i, Value::Bytes(value_str.as_bytes().to_vec().into())))
}

fn parse_bool(input: &str) -> IResult<&str, Value> {
    let (i, _) = tag("BoolType(source=")(input)?;
    let (i, value_str) = alt((tag("False"), tag("True")))(i)?;
    let value = match value_str {
        "False" => false,
        "True" => true,
        _ => unreachable!(),
    };
    Ok((i, Value::Bool(value)))
}

fn parse_list(input: &str) -> IResult<&str, Value> {
    let (i, _) = tag("[]")(input)?; // only empty list supported for now
    Ok((i, Value::List(vec![].into())))
}

fn parse_map(input: &str) -> IResult<&str, Value> {
    let (i, _) = tag("MapType({})")(input)?; // only empty map supported for now
    Ok((i, Value::Map(Map::default())))
}

fn parse_none(input: &str) -> IResult<&str, Value> {
    let (input, _) = tag("None")(input)?;
    Ok((input, Value::Null))
}

#[test]
fn test_parse_value_type() {
    let value = parse_value("IntType(source=123)").unwrap();
    assert_eq!(value, Value::Int(123));
}

#[test]
fn test_parse_int_type() {
    let (_, value) = parse_int("IntType(source=-9223372036854775808)").unwrap();
    assert_eq!(value, Value::Int(-9223372036854775808));
}

#[test]
fn test_parse_string_type() {
    let (_, value) = parse_string("StringType(source='!')").unwrap();
    assert_eq!(value, Value::String(Arc::new("!".to_string())));
}

#[test]
fn test_parse_bool_type() {
    let (_, value) = parse_bool("BoolType(source=False)").unwrap();
    assert_eq!(value, Value::Bool(false));
}
