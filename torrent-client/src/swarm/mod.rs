mod storage;
mod worker;

use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use crate::picker::{Block, Picker};
use crate::torrent::{Info, Metainfo, TorrentFile};
use crate::PEER_PREFIX;
use storage::{AssemblerHandle, StorageMessage};

use anyhow::Result;
use bytes::Bytes;
use crossbeam_utils::atomic::AtomicCell;
use rand::{
    distributions::{Alphanumeric, Distribution},
    thread_rng,
};
use tokio::{
    net::lookup_host,
    sync::{mpsc, Notify},
    task::{self, JoinHandle},
    time::timeout,
};
use tracing::{error, info, info_span, trace, trace_span};
use tracing_futures::Instrument;

#[derive(Debug)]
enum WorkerResponse {
    BlockRecieved {
        block: Block,
        data: Bytes,
    },
    Sleep {
        waker: Notify,
    },
    Shutdown {
        addr: SocketAddr,
        reason: Result<()>,
    },
}

#[derive(Debug)]
pub struct SwarmHandle {
    pub peer_id: [u8; 20],
    pub tracker_id: Option<String>,
    pub infohash: [u8; 20],
    pub announce: String,
    uploaded: Arc<AtomicCell<u64>>,
    downloaded: Arc<AtomicCell<u64>>,
    picker: Arc<Picker>,
    handle: JoinHandle<()>,
}

impl SwarmHandle {
    pub fn downloaded(&self) -> u64 {
        self.downloaded.load()
    }

    pub fn uploaded(&self) -> u64 {
        self.uploaded.load()
    }

    pub fn left(&self) -> u64 {
        self.picker.left()
    }

    pub fn size(&self) -> u64 {
        self.picker.size
    }

    pub async fn close(self) -> Result<()> {
        self.handle.await?;

        Ok(())
    }
}

pub async fn spawn(metainfo: Metainfo) -> Result<SwarmHandle> {
    let peer_id = {
        let mut buf = [0; 20];

        let mut peer_id = PEER_PREFIX
            .iter()
            .copied()
            .chain(Alphanumeric.sample_iter(thread_rng()).map(|a| a as u8));

        assert_eq!(
            peer_id
                .by_ref()
                .zip(buf.as_mut())
                .fold(0, |count, (byte, slot)| {
                    *slot = byte;
                    count + 1
                }),
            20
        );

        buf
    };

    let files;
    let assembler;
    let picker;

    match metainfo.info {
        Info::SingleFile {
            piece_length,
            pieces,
            name,
            length,
        } => {
            files = vec![TorrentFile {
                path: PathBuf::from(name.clone()),
                length,
            }];
            picker = {
                let piece_count = pieces.len() as u32;
                let picker = Picker::new(piece_length, piece_count, length);

                for index in 0..piece_count {
                    picker.add_piece(index);
                }

                Arc::new(picker)
            };
            assembler = storage::spawn(String::from("."), pieces, files, picker.clone()).await?;
        }
        Info::MultiFile { .. } => unimplemented!(),
    }

    let (tx, rx) = mpsc::channel(100);
    let mut workers = HashMap::new();

    for addr in lookup_host("172.21.176.1:39595").await? {
        let worker = timeout(
            Duration::from_secs(1),
            worker::spawn(
                &peer_id,
                &metainfo.infohash,
                addr,
                tx.clone(),
                picker.clone(),
            )
            .instrument(info_span!("worker", peer = ?addr)),
        )
        .await??;

        workers.insert(addr, worker);
    }

    let uploaded = Arc::new(AtomicCell::new(0));
    let downloaded = Arc::new(AtomicCell::new(0));

    let handle = task::spawn({
        let uploaded = uploaded.clone();
        let downloaded = downloaded.clone();
        let picker = picker.clone();

        async move {
            trace!("swarm starting up");
            work(rx, picker, assembler, workers, uploaded, downloaded)
                .instrument(trace_span!("work"))
                .await;
            trace!("swarm shutting down");
        }
        .in_current_span()
    });

    Ok(SwarmHandle {
        peer_id,
        tracker_id: None,
        infohash: metainfo.infohash,
        announce: metainfo.announce,
        uploaded,
        downloaded,
        picker,
        handle,
    })
}

async fn work(
    mut worker_channel: mpsc::Receiver<WorkerResponse>,
    work_queue: Arc<Picker>,
    mut assembler: AssemblerHandle,
    mut workers: HashMap<SocketAddr, JoinHandle<()>>,
    uploaded: Arc<AtomicCell<u64>>,
    downloaded: Arc<AtomicCell<u64>>,
) {
    while let Some(message) = worker_channel.recv().await {
        match message {
            WorkerResponse::BlockRecieved { block, data } => {
                trace!("recieved block ({}, {})", block.index, block.begin);
                downloaded.fetch_add(data.len() as u64);
                assembler
                    .send(StorageMessage::SubmitBlock { block, data })
                    .await
                    .unwrap_or_else(|e| error!("{}", e));
            }
            WorkerResponse::Sleep { .. } => {} //todo!("sleep"),
            WorkerResponse::Shutdown { addr, reason } => {
                if let Err(e) = reason {
                    info!("worker {:?} terminated: {:#?}", addr, e);
                }

                workers.remove(&addr);
            }
        }
    }
}
