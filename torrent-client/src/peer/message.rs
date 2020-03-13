use anyhow::bail;
use bytes::{Buf, BufMut, BytesMut};
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use tokio_util::codec::{Decoder, Encoder};

#[derive(FromPrimitive, ToPrimitive, Debug)]
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
    pub payload: Option<BytesMut>,
}

#[derive(Debug)]
pub struct MessageCodec {}

impl Decoder for MessageCodec {
    type Item = Message;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if src.len() < 4 {
            return Ok(None);
        }

        let n = {
            let n = src.get_u32();

            if n < 1 {
                bail!("Zero length payload.");
            }

            n as usize
        };

        src.reserve(n);

        if src.len() < n {
            return Ok(None);
        }

        let id = match MessageID::from_u8(src.get_u8()) {
            Some(a) => a,
            _ => bail!("Invalid message id."),
        };

        let payload = match n {
            1 => None,
            _ => Some(src.split_to(n - 1)),
        };

        src.reserve(4);

        Ok(Some(Message { id, payload }))
    }
}

impl Encoder<Message> for MessageCodec {
    type Error = anyhow::Error;

    fn encode(&mut self, data: Message, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let n = 1 + match &data.payload {
            Some(a) => a.len(),
            None => 0,
        };

        dst.reserve(4 + n);

        dst.put_u8(match data.id.to_u8() {
            Some(a) => a,
            _ => bail!("Invalid message id."),
        });

        if let Some(payload) = data.payload {
            dst.extend_from_slice(&payload[..])
        }

        Ok(())
    }
}
