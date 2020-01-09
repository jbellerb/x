use super::{info, key_value_if, key_value_partial, string_obj, Metainfo};

use crypto::digest::Digest;
use crypto::sha1::Sha1;

use nom::{
    character::complete::char,
    combinator::all_consuming,
    multi::{many0, many_till},
    sequence::{delimited, tuple},
    IResult,
};

pub fn parse(i: &[u8]) -> IResult<&[u8], Metainfo> {
    all_consuming(delimited(char('d'), metainfo, char('e')))(i)
}

fn metainfo(i: &[u8]) -> IResult<&[u8], Metainfo> {
    match tuple((
        many_till(key_value_partial, key_value_if("announce")),
        many_till(key_value_partial, key_value_if("info")),
        many0(key_value_partial),
    ))(i)
    {
        Ok((remaining_input, ((_, (_, announce)), (_, (_, info_raw)), _))) => {
            let info = match all_consuming(info::parse)(info_raw) {
                Ok((_, a)) => a,
                Err(e) => return Err(e),
            };

            let mut hasher = Sha1::new();
            hasher.input(info_raw);

            let mut infohash = [0; 20];
            hasher.result(&mut infohash);

            Ok((
                remaining_input,
                Metainfo {
                    info,
                    infohash,
                    announce: all_consuming(string_obj)(announce)?.1,
                },
            ))
        }
        Err(e) => Err(e),
    }
}
