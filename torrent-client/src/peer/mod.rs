use std::net::SocketAddr;

use anyhow::{ensure, Result};
use tokio::net::TcpStream;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use bytes::{Bytes, BytesMut, BufMut};

#[derive(Debug)]
pub struct Peer {
    pub peer_id: Bytes,
    pub socket: TcpStream,
    pub peer_choke: bool,
    pub self_choke: bool,
    pub peer_interest: bool,
    pub self_interest: bool,
}

impl Peer {
    pub async fn handshake(peer_id: &[u8; 20], infohash: &[u8; 20], address: SocketAddr) -> Result<Peer> {
        let mut socket = TcpStream::connect(address).await?;

        let mut buf = BytesMut::with_capacity(68);
        buf.put_u8(19);
        buf.put(&b"BitTorrent protocol"[..]);
        buf.put_u64(0);
        buf.put(&infohash[..]);
        buf.put(&peer_id[..]);

        socket.write_all(&buf).await?;

        ensure!(socket.read(&mut buf).await? == 68, "Invalid handshake.");
        ensure!(&buf[1..20] == b"BitTorrent protocol", "Invalid protocol.");
        ensure!(&buf[28..48] == infohash, "Wrong infohash.");

        Ok(Peer{
            peer_id: Bytes::copy_from_slice(&buf[48..68]),
            socket,
            peer_choke: true,
            self_choke: true,
            peer_interest: false,
            self_interest: false,
        })
    }
}
