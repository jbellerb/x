mod comms;
mod socket;

use std::collections::HashSet;
use std::net::SocketAddr;

use crate::picker::Block;
use crate::protocol::{Bitfield, Frame, MAX_PENDING};
use socket::PeerConnection;

use anyhow::{anyhow, ensure, Result};
use bytes::{BufMut, Bytes, BytesMut};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::TcpStream,
};
use tracing::{trace, trace_span};
use tracing_futures::Instrument;

#[derive(Debug)]
pub struct Peer {
    pub peer_id: Bytes,
    socket: PeerConnection,
    pub address: SocketAddr,
    pub peer_choke: bool,
    pub self_choke: bool,
    pub peer_interest: bool,
    pub self_interest: bool,
    pub bitfield: Bitfield,
    pub pending_blocks: HashSet<Block>,
}

impl Peer {
    pub async fn new(
        self_id: &[u8; 20],
        infohash: &[u8; 20],
        pieces: u32,
        address: SocketAddr,
    ) -> Result<Peer> {
        let (peer_id, mut socket) = handshake(self_id, infohash, &address)
            .instrument(trace_span!("handshake"))
            .await?;
        let bitfield = get_bitfield(&mut socket, pieces)
            .instrument(trace_span!("get_bitfield"))
            .await?;

        Ok(Peer {
            peer_id,
            socket,
            address,
            peer_choke: true,
            self_choke: true,
            peer_interest: false,
            self_interest: false,
            bitfield,
            pending_blocks: HashSet::with_capacity(MAX_PENDING),
        })
    }

    pub async fn next(&mut self) -> Option<Result<Frame>> {
        self.socket.next().await
    }
}

async fn handshake(
    self_id: &[u8; 20],
    infohash: &[u8; 20],
    address: &SocketAddr,
) -> Result<(Bytes, PeerConnection)> {
    trace!("attempting to connect");
    let mut socket = TcpStream::connect(address).await?;

    let mut buf = BytesMut::with_capacity(68);
    buf.put_u8(19);
    buf.put(&b"BitTorrent protocol"[..]);
    buf.put_u64(0);
    buf.put(&infohash[..]);
    buf.put(&self_id[..]);

    trace!("sending handshake");
    socket.write_all(&buf).await?;

    ensure!(
        socket.read_exact(&mut buf).await? == 68,
        "handshake is the wrong size"
    );
    ensure!(&buf[1..20] == b"BitTorrent protocol", "invalid protocol.");
    ensure!(
        &buf[28..48] == infohash,
        "wrong infohash. got {:?}, expected {:?}",
        &buf[28..48],
        infohash
    );

    trace!("successful handshake with {}", address);

    Ok((
        Bytes::copy_from_slice(&buf[48..68]),
        PeerConnection::new(socket),
    ))
}

async fn get_bitfield(peer: &mut PeerConnection, pieces: u32) -> Result<Bitfield> {
    if let Some(Ok(Frame::Bitfield { bitfield })) = peer.next().await {
        let block_size = (pieces as usize + 7) / 8;
        let upper_mask = 0xff >> (pieces as usize % 8);

        ensure!(
            bitfield.len() == block_size,
            "bitfield is the wrong size. got {} bytes, expected {} bytes",
            bitfield.len(),
            block_size,
        );
        ensure!(
            bitfield[block_size - 1] & upper_mask == 0,
            "bitfield has upper bits set."
        );

        trace!("recieved good bitfield");

        Ok(Bitfield::from_bytes(bitfield, pieces as usize))
    } else {
        Err(anyhow!("no bitfield given."))
    }
}
