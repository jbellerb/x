use std::net::SocketAddr;
use std::sync::Arc;

use super::WorkerResponse;
use crate::peer::Peer;
use crate::picker::{Block, Picker};
use crate::protocol::{Frame, MAX_PENDING};

use anyhow::{bail, Result};
use tokio::{
    sync::{mpsc, Notify},
    task::{self, JoinHandle},
};
use tracing::{debug, error, trace, trace_span};
use tracing_futures::Instrument;

pub(super) async fn spawn(
    self_id: &[u8; 20],
    infohash: &[u8; 20],
    addr: SocketAddr,
    mut channel: mpsc::Sender<WorkerResponse>,
    picker: Arc<Picker>,
) -> Result<JoinHandle<()>> {
    trace!("worker starting up");
    let peer = match Peer::new(self_id, infohash, picker.pieces(), addr)
        .instrument(trace_span!("init_peer"))
        .await
    {
        Ok(a) => a,
        Err(e) => {
            error!("{}. shutting down", e);
            return Err(e);
        }
    };

    Ok(task::spawn(
        async move {
            let piece_size = picker.piece_size;
            match work(peer, &mut channel, picker, piece_size)
                .instrument(trace_span!("work"))
                .await
            {
                Err(e) => channel
                    .send(WorkerResponse::Shutdown {
                        addr,
                        reason: Err(e),
                    })
                    .await
                    .unwrap(),
                Ok(()) => channel
                    .send(WorkerResponse::Shutdown {
                        addr,
                        reason: Ok(()),
                    })
                    .await
                    .unwrap(),
            }
            trace!("worker shutting down");
        }
        .in_current_span(),
    ))
}

async fn work(
    mut peer: Peer,
    channel: &mut mpsc::Sender<WorkerResponse>,
    picker: Arc<Picker>,
    piece_size: u32,
) -> Result<()> {
    peer.interested().await?;

    loop {
        if !peer.peer_choke && peer.pending_blocks.len() < MAX_PENDING {
            if let Some(block) = picker.pop() {
                trace!("selected block {}", block);

                if !peer.bitfield.get(block.index as usize) {
                    trace!("block {} not available, returning to picker", block);
                    picker.push(block);
                    continue;
                }

                peer.request(&block).await?;
                peer.pending_blocks.insert(block);
            } else if !peer.pending_blocks.is_empty() {
                listen(&mut peer, channel).await?;
            } else {
                let waker = Notify::new();

                channel.send(WorkerResponse::Sleep { waker }).await?;
            }
        } else {
            listen(&mut peer, channel).await?;
        }
    }
}

async fn listen(peer: &mut Peer, channel: &mut mpsc::Sender<WorkerResponse>) -> Result<()> {
    if let Some(message) = peer.next().instrument(trace_span!("recieve")).await {
        match message? {
            Frame::Choke => {
                trace!("choked");
                peer.peer_choke = true;
            }
            Frame::Unchoke => {
                trace!("unchoked");
                peer.peer_choke = false;
            }
            Frame::Interested => {
                trace!("peer interested");
                peer.peer_interest = true;
            }
            Frame::NotInterested => {
                trace!("peer no longer interested");
                peer.peer_interest = false;
            }
            Frame::Have { piece } => {
                trace!("peer has piece {}", piece);
                peer.bitfield.set(piece as usize, true);
            }
            Frame::Request { .. } => bail!("not implemented: request"),
            Frame::Piece {
                index,
                begin,
                block,
            } => {
                let block_info = Block {
                    index,
                    begin,
                    length: block.len() as u32,
                };

                if peer.pending_blocks.remove(&block_info) {
                    debug!("recieved block ({}+{})", index, begin);
                    channel
                        .send(WorkerResponse::BlockRecieved {
                            block: block_info,
                            data: block,
                        })
                        .await?;
                } else {
                    bail!("unknown block recieved. terminating connection");
                }
            }
            Frame::Cancel { .. } => bail!("Not implemented: cancel"),
            _ => bail!("bitfield already provided"),
        }
    }

    Ok(())
}
