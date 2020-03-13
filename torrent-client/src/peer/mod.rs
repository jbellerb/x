mod bitfield;
mod message;

use std::convert::TryInto;
use std::net::SocketAddr;

use anyhow::{ensure, Result};
use bytes::{BufMut, Bytes, BytesMut};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::stream::StreamExt;
use tokio_util::codec::Framed;

use super::torrent::{Info, Torrent};
use bitfield::Bitfield;
use message::{MessageCodec, MessageID};

#[derive(Debug)]
pub struct Peer<'a> {
    pub peer_id: Bytes,
    pub socket: Framed<TcpStream, MessageCodec>,
    pub peer_choke: bool,
    pub self_choke: bool,
    pub peer_interest: bool,
    pub self_interest: bool,
    pub bitfield: Bitfield,
    pub torrent: &'a Torrent,
}

impl<'a> Peer<'a> {
    pub async fn handshake(
        peer_id: &[u8; 20],
        torrent: &'a Torrent,
        address: SocketAddr,
    ) -> Result<Peer<'a>> {
        let mut socket = TcpStream::connect(address).await?;

        let mut buf = BytesMut::with_capacity(68);
        buf.put_u8(19);
        buf.put(&b"BitTorrent protocol"[..]);
        buf.put_u64(0);
        buf.put(&torrent.metainfo.infohash[..]);
        buf.put(&peer_id[..]);

        socket.write_all(&buf).await?;

        ensure!(socket.read(&mut buf).await? == 68, "Invalid handshake.");
        ensure!(&buf[1..20] == b"BitTorrent protocol", "Invalid protocol.");
        ensure!(buf[28..48] == torrent.metainfo.infohash, "Wrong infohash.");

        Ok(Peer {
            peer_id: Bytes::copy_from_slice(&buf[48..68]),
            socket: Framed::new(socket, MessageCodec {}),
            peer_choke: true,
            self_choke: true,
            peer_interest: false,
            self_interest: false,
            bitfield: Bitfield(BytesMut::with_capacity(match torrent.metainfo.info {
                Info::SingleFile { length, .. } => length.try_into()?,
                Info::MultiFile { .. } => unimplemented!(),
            })),
            torrent,
        })
    }

    pub async fn listen(&mut self) {
        while let Some(Ok(message)) = self.socket.next().await {
            println!("{:#?}", message);
            match message.id {
                MessageID::Choke => self.peer_choke = true,
                MessageID::Unchoke => self.peer_choke = false,
                MessageID::Interested => self.peer_interest = true,
                MessageID::NotInterested => self.peer_interest = false,
                MessageID::Have => todo!("have"),
                MessageID::Bitfield => match message.payload {
                    Some(a) => self.bitfield = Bitfield(a),
                    _ => continue,
                },
                MessageID::Request => todo!("request"),
                MessageID::Piece => todo!("piece"),
                MessageID::Cancel => todo!("cancel"),
            }
        }
    }
}
