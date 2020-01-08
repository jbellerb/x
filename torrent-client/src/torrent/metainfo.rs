use serde::{Deserialize, Serialize};
use serde_bencode::{de, error, ser};

use crypto::digest::Digest;
use crypto::sha1::Sha1;

use super::bencode::{bencode_value, integer, list, string, BencodeValue};
use super::info::Info;

use std::collections::HashMap;
use std::num::ParseIntError;
use std::str;
use std::string::{FromUtf8Error, String};

use nom::{
    character::complete::char,
    bytes::complete::take,
    combinator::{cut, map, map_res, recognize},
    multi::many0,
    sequence::{delimited, tuple},
    IResult,
};

#[derive(Deserialize, Debug)]
struct RawMetainfo {
    info: Info,
    announce: String,
}

#[derive(Serialize, Debug)]
pub struct Metainfo {
    info: Info,
    #[serde(skip)]
    infohash: [u8; 20],
    announce: String,
}

impl Metainfo {
    pub fn from_torrent(torrent: &[u8]) -> Result<Metainfo, &'static str> {
        match delimited(
            char('d'),
            cut(map_res(many0(key_value_partial), parse_metainfo)),
            char('e'),
        )(torrent) {
            Ok(([], a)) => Ok(a),
            _ => Err("failed to parse info")
        }
    }
}

fn bytes_to_string(i: Vec<u8>) -> Result<String, &'static str> {
    match String::from_utf8(i) {
        Ok(a) => Ok(a),
        Err(_) => return Err("invalid name")
    }
}

fn piece_hashes(i: &[u8]) -> IResult<&[u8], Vec<[u8; 20]>> {
    map(many0(take(20usize)), |hashes: Vec<&[u8]>| hashes.into_iter().map(|hash| {
        let mut bytes = hash.into_iter();
        let mut chunk = [0; 20];
        println!("{:?}", hash);

        #[allow(clippy::needless_range_loop)]
        for i in 0..20 {
            // take guarantees that there will always be 20
            chunk[i] = *bytes.next().unwrap();
        };

        chunk
    }).collect())(i)
}

fn key_value_partial(i: &[u8]) -> IResult<&[u8], (String, &[u8])> {
    tuple((
        map_res(string, |key| String::from_utf8(key.to_vec())),
        recognize(bencode_value),
    ))(i)
}

fn get_field<K, V>(parser: &dyn Fn(&[u8]) -> IResult<&[u8], V>, fields: &HashMap<K, &[u8]>, key: &K) -> Result<V, &'static str>
where
    K: std::cmp::Eq + std::hash::Hash
{
    match HashMap::get(fields, key) {
        Some(a) => match parser(a) {
            Ok(([], val)) => Ok(val),
            _ => Err("invalid data for key")
        }
        _ => Err("key not present")
    }
}

fn info(i: &[u8]) -> Result<Info, &'static str> {
    match delimited(
        char('d'),
        cut(map_res(many0(key_value_partial), parse_info)),
        char('e'),
    )(i) {
        Ok(([], a)) => Ok(a),
        _ => Err("failed to parse info")
    }
}

fn parse_info(entries: Vec<(String, &[u8])>) -> Result<Info, &'static str> {
    let fields: HashMap<String, &[u8]> = entries.into_iter().collect();

    let piece_length = get_field(&integer, &fields, &String::from("piece length"))?;
    let name = bytes_to_string(get_field(&string, &fields, &String::from("name"))?)?;
    let pieces = get_field(&piece_hashes, & fields, &String::from("pieces"))?;

    if fields.contains_key("length") {
        Ok(Info::SingleFile {
            piece_length,
            pieces,
            name,
            length: get_field(&integer, &fields, &String::from("length"))?,
        })
    } else {
        Ok(Info::MultiFile {
            piece_length,
            pieces,
            name,
            files: Vec::new(),
        })
    }
}

fn parse_metainfo(entries: Vec<(String, &[u8])>) -> Result<Metainfo, &'static str> {
    let fields: HashMap<String, &[u8]> = entries.into_iter().collect();

    let info_raw = match fields.get("info") {
        Some(a) => a,
        None => return Err("torrent missing info")
    };

    let mut hasher = Sha1::new();
    hasher.input(info_raw);

    let mut infohash = [0; 20];
    hasher.result(&mut infohash);

    Ok(Metainfo {
        info: info(info_raw)?,
        infohash,
        announce: bytes_to_string(get_field(&string, &fields, &String::from("announce"))?)?
    })
}
