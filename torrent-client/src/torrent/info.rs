use std::convert::TryFrom;
use std::string::String;

use super::bencode::{integer, string};
use super::{key_value_if, key_value_partial, string_obj, Info, TorrentFile};

use nom::{
    branch::alt,
    bytes::complete::take,
    character::complete::char,
    combinator::{all_consuming, cut, map, map_res},
    multi::{many0, many1, many_till},
    sequence::{delimited, tuple},
    IResult,
};

pub fn parse(i: &[u8]) -> IResult<&[u8], Info> {
    delimited(char('d'), cut(alt((info_single, info_multi))), char('e'))(i)
}

fn torrent_file(i: &[u8]) -> IResult<&[u8], TorrentFile> {
    match delimited(
        char('d'),
        cut(tuple((
            many_till(key_value_partial, key_value_if("length")),
            many_till(key_value_partial, key_value_if("path")),
            many0(key_value_partial),
        ))),
        char('e'),
    )(i)
    {
        Ok((remaining_input, ((_, (_, length)), (_, (_, path)), _))) => {
            let (_, path) = map(
                all_consuming(delimited(char('l'), many1(string_obj), char('e'))),
                |parts| {
                    let mut parts = parts.iter();
                    let mut path = String::new();

                    if let Some(part) = parts.next() {
                        path.push_str(part);

                        for part in parts {
                            path.push_str("/");
                            path.push_str(part);
                        }
                    };

                    path
                },
            )(path)?;

            Ok((
                remaining_input,
                TorrentFile {
                    length: map_res(all_consuming(integer), u64::try_from)(length)?.1,
                    path,
                },
            ))
        }
        Err(e) => Err(e),
    }
}

fn piece_hashes(i: &[u8]) -> IResult<&[u8], Vec<[u8; 20]>> {
    let (_, i) = all_consuming(string)(i)?;

    many1(map(take(20usize), |piece| {
        let mut hash: [u8; 20] = Default::default();
        // Relying on take to make sure the slice is the correct length
        hash.copy_from_slice(piece);

        hash
    }))(i)
}

fn info_single(i: &[u8]) -> IResult<&[u8], Info> {
    match tuple((
        many_till(key_value_partial, key_value_if("length")),
        many_till(key_value_partial, key_value_if("name")),
        many_till(key_value_partial, key_value_if("piece length")),
        many_till(key_value_partial, key_value_if("pieces")),
        many0(key_value_partial),
    ))(i)
    {
        Ok((
            remaining_input,
            ((_, (_, length)), (_, (_, name)), (_, (_, piece_length)), (_, (_, pieces)), _),
        )) => Ok((
            remaining_input,
            Info::SingleFile {
                piece_length: map_res(all_consuming(integer), u64::try_from)(piece_length)?.1,
                pieces: all_consuming(piece_hashes)(pieces)?.1,
                name: all_consuming(string_obj)(name)?.1,
                length: map_res(all_consuming(integer), u64::try_from)(length)?.1,
            },
        )),
        Err(e) => Err(e),
    }
}

fn info_multi(i: &[u8]) -> IResult<&[u8], Info> {
    match tuple((
        many_till(key_value_partial, key_value_if("files")),
        many_till(key_value_partial, key_value_if("name")),
        many_till(key_value_partial, key_value_if("piece length")),
        many_till(key_value_partial, key_value_if("pieces")),
        many0(key_value_partial),
    ))(i)
    {
        Ok((
            remaining_input,
            ((_, (_, files)), (_, (_, name)), (_, (_, piece_length)), (_, (_, pieces)), _),
        )) => {
            let (_, files) =
                all_consuming(delimited(char('l'), cut(many1(torrent_file)), char('e')))(files)?;

            Ok((
                remaining_input,
                Info::MultiFile {
                    piece_length: map_res(all_consuming(integer), u64::try_from)(piece_length)?.1,
                    pieces: all_consuming(piece_hashes)(pieces)?.1,
                    name: all_consuming(string_obj)(name)?.1,
                    files,
                },
            ))
        }
        Err(e) => Err(e),
    }
}
