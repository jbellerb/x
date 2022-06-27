use std::cmp::Ordering;
use std::collections::HashMap;
use std::num::ParseIntError;
use std::str;
use std::string::String;

use anyhow::{anyhow, Result};
use nom::{
    branch::alt,
    bytes::complete::take,
    character::complete::{char, digit1},
    combinator::{cut, map, map_res, opt},
    multi::many1,
    sequence::{delimited, terminated, tuple},
    IResult,
};

#[derive(Debug)]
pub enum BencodeValue {
    BString(Vec<u8>),
    BInteger(i64),
    BList(Vec<BencodeValue>),
    BDictionary(HashMap<String, BencodeValue>),
}

fn from_decimal<T>(i: &[u8]) -> Result<T>
where
    T: str::FromStr<Err = ParseIntError>,
{
    let input = str::from_utf8(i)?;

    match T::from_str(input) {
        Ok(int) => Ok(int),
        Err(_) => Err(anyhow!("Failed to parse \"{:?}\" as int.", input)),
    }
}

pub fn string(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (i, length): (&[u8], usize) = terminated(map_res(digit1, from_decimal), char(':'))(i)?;

    take(length)(i)
}

fn invert(i: i64) -> Result<i64> {
    if i > 0 {
        Ok(-i)
    } else {
        Err(anyhow!("Failed to invert parsed number: {}", i))
    }
}

fn signed(i: &[u8]) -> IResult<&[u8], i64> {
    let (i, sign) = opt(char('-'))(i)?;
    let mut num = map_res(digit1, from_decimal);

    match sign {
        Some(_) => map_res(num, invert)(i),
        None => num(i),
    }
}

pub fn integer(i: &[u8]) -> IResult<&[u8], i64> {
    delimited(char('i'), cut(signed), char('e'))(i)
}

pub fn list(i: &[u8]) -> IResult<&[u8], Vec<BencodeValue>> {
    delimited(char('l'), cut(many1(bencode_value)), char('e'))(i)
}

fn key_value(i: &[u8]) -> IResult<&[u8], (String, BencodeValue)> {
    tuple((
        map_res(string, |key| String::from_utf8(key.to_vec())),
        bencode_value,
    ))(i)
}

fn collect_if_sorted(
    entries: Vec<(String, BencodeValue)>,
) -> Result<HashMap<String, BencodeValue>> {
    let mut dictionary = HashMap::new();
    let mut last_key = String::new();

    for (key, value) in entries.into_iter() {
        match last_key.cmp(&key) {
            Ordering::Greater => return Err(anyhow!("Dictionary not sorted order.")),
            _ => {
                last_key = key.clone();
                dictionary.insert(key, value);
            }
        }
    }

    Ok(dictionary)
}

pub fn dictionary(i: &[u8]) -> IResult<&[u8], HashMap<String, BencodeValue>> {
    delimited(
        char('d'),
        cut(map_res(many1(key_value), collect_if_sorted)),
        char('e'),
    )(i)
}

pub fn bencode_value(i: &[u8]) -> IResult<&[u8], BencodeValue> {
    alt((
        map(dictionary, BencodeValue::BDictionary),
        map(list, BencodeValue::BList),
        map(string, |string| BencodeValue::BString(string.to_vec())),
        map(integer, BencodeValue::BInteger),
    ))(i)
}
