use super::Peer;
use crate::picker::Block;
use crate::protocol::Frame;

use anyhow::Result;

impl Peer {
    pub async fn choke(&mut self) -> Result<()> {
        if !self.peer_choke {
            self.socket.send(Frame::Choke).await?;
            self.peer_choke = true;
        }

        Ok(())
    }

    pub async fn unchoke(&mut self) -> Result<()> {
        if self.peer_choke {
            self.socket.send(Frame::Unchoke).await?;
            self.peer_choke = false;
        }

        Ok(())
    }

    pub async fn interested(&mut self) -> Result<()> {
        if !self.self_interest {
            self.socket.send(Frame::Interested).await?;
            self.self_interest = true;
        }

        Ok(())
    }

    pub async fn not_interested(&mut self) -> Result<()> {
        if self.self_interest {
            self.socket.send(Frame::NotInterested).await?;
            self.self_interest = false;
        }

        Ok(())
    }

    pub async fn request(&mut self, block: &Block) -> Result<()> {
        self.socket
            .send(Frame::Request {
                index: block.index,
                begin: block.begin,
                length: block.length,
            })
            .await
    }
}
