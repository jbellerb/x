pub mod bencode;
mod info;
mod metainfo;

use bencode::{bencode_value, string};
use std::path::PathBuf;

use anyhow::{anyhow, Result};
use nom::{
    combinator::{map_res, recognize, verify},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
pub struct Metainfo {
    pub info: Info,
    pub infohash: [u8; 20],
    pub announce: String,
}

impl Metainfo {
    pub fn from_bytes(bytes: &[u8]) -> Result<Metainfo> {
        match metainfo::parse(bytes) {
            Ok(([], metainfo)) => Ok(metainfo),
            Err(_) => Err(anyhow!("Failed to parse torrent.")),
            _ => unreachable!(), // The remaining slice of the input should
                                 // always be empty as parse is all_consuming.
        }
    }
}

#[derive(Debug)]
pub enum Info {
    SingleFile {
        piece_length: u32,
        pieces: Vec<[u8; 20]>,
        name: String,
        length: u64,
    },
    MultiFile {
        piece_length: u32,
        pieces: Vec<[u8; 20]>,
        name: String,
        files: Vec<TorrentFile>,
    },
}

#[derive(Debug)]
pub struct TorrentFile {
    pub length: u64,
    pub path: PathBuf,
}

// Shared parsers

fn key_value_partial(i: &[u8]) -> IResult<&[u8], (String, &[u8])> {
    tuple((
        map_res(string, |key| String::from_utf8(key.to_vec())),
        recognize(bencode_value),
    ))(i)
}

fn key_value_if<'a>(
    key: &'static str,
) -> impl Fn(&'a [u8]) -> IResult<&'a [u8], (String, &'a [u8])> {
    move |i: &[u8]| {
        tuple((
            map_res(
                verify(string, |found: &[u8]| found == key.as_bytes()),
                |key| String::from_utf8(key.to_vec()),
            ),
            recognize(bencode_value),
        ))(i)
    }
}

fn string_obj(i: &[u8]) -> IResult<&[u8], String> {
    map_res(string, |string| String::from_utf8(string.to_vec()))(i)
}
