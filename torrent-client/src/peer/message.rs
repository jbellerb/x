use std::io;

use anyhow::bail;
use bytes::{Buf, Bytes, BytesMut};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use tokio_util::codec::{Decoder, Encoder};

#[derive(FromPrimitive, Debug)]
pub enum MessageID {
    Choke,
    Unchoke,
    Interested,
    NotInterested,
    Have,
    Bitfield,
    Request,
    Piece,
    Cancel,
}

#[derive(Debug)]
pub struct Message {
    pub id: MessageID,
    pub payload: Option<Bytes>,
}

#[derive(Debug)]
pub struct MessageCodec {}

impl Decoder for MessageCodec {
    type Item = Message;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        let n = src.get_u32();

        if n < 1 {
            bail!("Zero length payload.");
        }

        let n = (n - 1) as usize;

        src.reserve(n);

        if src.len() < n {
            return Ok(None);
        }

        let id = match MessageID::from_u8(src.get_u8()) {
            Some(a) => a,
            _ => bail!("Invalid message id."),
        };
        let payload = match n {
            0 => None,
            _ => Some(src.split_to(n).freeze()),
        };

        src.reserve(4);

        Ok(Some(Message { id, payload }))
    }
}
