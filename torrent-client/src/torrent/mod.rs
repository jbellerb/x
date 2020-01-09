mod bencode;
mod info;
mod metainfo;

use bencode::{bencode_value, string};

use nom::{
    combinator::{map_res, recognize, verify},
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
pub struct Metainfo {
    info: Info,
    infohash: [u8; 20],
    announce: String,
}

impl Metainfo {
    pub fn from_torrent(torrent: &[u8]) -> Result<Metainfo, &'static str> {
        match metainfo::parse(torrent).unwrap() {
            ([], a) => Ok(a),
            _ => Err("failed to parse torrent"),
        }
    }
}

#[derive(Debug)]
pub enum Info {
    SingleFile {
        piece_length: i64,
        pieces: Vec<[u8; 20]>,
        name: String,
        length: i64,
    },
    MultiFile {
        piece_length: i64,
        pieces: Vec<[u8; 20]>,
        name: String,
        files: Vec<TorrentFile>,
    },
}

#[derive(Debug)]
pub struct TorrentFile {
    length: i64,
    path: String,
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
