use crate::protocol::{Codec, Frame};

use anyhow::Result;
use futures::{sink::SinkExt, stream::StreamExt};
use tokio::net::TcpStream;
use tokio_util::codec::Framed;

#[derive(Debug)]
pub struct PeerConnection {
    socket: Framed<TcpStream, Codec>,
}

impl PeerConnection {
    pub(super) fn new(socket: TcpStream) -> PeerConnection {
        PeerConnection {
            socket: Framed::new(socket, Codec::new()),
        }
    }
}

impl PeerConnection {
    pub(super) async fn next(&mut self) -> Option<Result<Frame>> {
        self.socket.next().await
    }

    // TODO: Batch Requests
    pub async fn send(&mut self, frame: Frame) -> Result<()> {
        self.socket.send(frame).await
    }
}
