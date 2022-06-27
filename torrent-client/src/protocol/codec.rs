use std::convert::TryInto;

use anyhow::bail;
use bytes::{Buf, BufMut, Bytes, BytesMut};
use tokio_util::codec::{Decoder, Encoder};

#[derive(Clone, Debug)]
pub enum Frame {
    Choke,
    Unchoke,
    Interested,
    NotInterested,
    Have {
        piece: u32,
    },
    Bitfield {
        bitfield: BytesMut,
    },
    Request {
        index: u32,
        begin: u32,
        length: u32,
    },
    Piece {
        index: u32,
        begin: u32,
        block: Bytes,
    },
    Cancel {
        index: u32,
        begin: u32,
        length: u32,
    },
}

#[derive(Debug)]
pub struct Codec {
    working: bool,
    frame_size: usize,
}

impl Codec {
    pub fn new() -> Codec {
        Codec {
            working: false,
            frame_size: 0,
        }
    }
}

impl Decoder for Codec {
    type Item = Frame;
    type Error = anyhow::Error;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if src.len() < 4 {
            return Ok(None);
        }

        if !self.working {
            self.frame_size = {
                let n = src.get_u32();

                if n < 1 {
                    bail!("zero length payload");
                }

                n as usize
            };
            self.working = true;
        }

        src.reserve(self.frame_size + 4);

        if src.len() < self.frame_size {
            return Ok(None);
        }

        let message = match src.get_u8() {
            0 => Frame::Choke,
            1 => Frame::Unchoke,
            2 => Frame::Interested,
            3 => Frame::NotInterested,
            4 => Frame::Have {
                piece: src.get_u32(),
            },
            5 => Frame::Bitfield {
                bitfield: src.split_to(self.frame_size - 1),
            },
            6 => Frame::Request {
                index: src.get_u32(),
                begin: src.get_u32(),
                length: src.get_u32(),
            },
            7 => Frame::Piece {
                index: src.get_u32(),
                begin: src.get_u32(),
                block: src.split_to(self.frame_size - 9).freeze(),
            },
            8 => Frame::Cancel {
                index: src.get_u32(),
                begin: src.get_u32(),
                length: src.get_u32(),
            },
            _ => bail!("invalid message id"),
        };

        self.working = false;

        Ok(Some(message))
    }
}

impl Encoder<Frame> for Codec {
    type Error = anyhow::Error;

    fn encode(&mut self, data: Frame, dst: &mut BytesMut) -> Result<(), Self::Error> {
        let n = match &data {
            Frame::Have { .. } => 5,
            Frame::Request { .. } | Frame::Cancel { .. } => 13,
            Frame::Bitfield { bitfield: a } => a.len() + 1,
            Frame::Piece { block, .. } => block.len() + 1,
            _ => 1,
        };

        dst.reserve(4 + n);

        dst.put_u32(n.try_into()?);

        dst.put_u8(match data {
            Frame::Choke => 0,
            Frame::Unchoke => 1,
            Frame::Interested => 2,
            Frame::NotInterested => 3,
            Frame::Have { .. } => 4,
            Frame::Bitfield { .. } => 5,
            Frame::Request { .. } => 6,
            Frame::Piece { .. } => 7,
            Frame::Cancel { .. } => 8,
        });

        match data {
            Frame::Have { piece } => dst.put_u32(piece),
            Frame::Bitfield { bitfield } => dst.extend_from_slice(&bitfield),
            Frame::Request {
                index,
                begin,
                length,
            }
            | Frame::Cancel {
                index,
                begin,
                length,
            } => {
                dst.put_u32(index);
                dst.put_u32(begin);
                dst.put_u32(length);
            }
            Frame::Piece {
                index,
                begin,
                block,
            } => {
                dst.put_u32(index);
                dst.put_u32(begin);
                dst.extend_from_slice(&block[..])
            }
            _ => {}
        }

        Ok(())
    }
}
