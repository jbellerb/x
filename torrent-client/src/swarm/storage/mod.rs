mod lookup;

use std::collections::{BTreeMap, HashMap};
use std::io::SeekFrom;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use super::{Block, Picker};
use crate::torrent::TorrentFile;
use lookup::FileMap;

use anyhow::Result;
use bytes::{BufMut, Bytes, BytesMut};
use crypto::{digest::Digest, sha1::Sha1};
use tokio::{
    fs::OpenOptions,
    io::AsyncWriteExt,
    sync::mpsc,
    task::{self, JoinHandle},
};
use tracing::{debug, error, trace, trace_span};
use tracing_futures::Instrument;

#[derive(Debug)]
pub enum StorageMessage {
    SubmitBlock { block: Block, data: Bytes },
    Shutdown,
}

#[derive(Debug)]
pub struct AssemblerHandle {
    handle: JoinHandle<()>,
    tx: mpsc::Sender<StorageMessage>,
}

impl AssemblerHandle {
    pub async fn send(&mut self, value: StorageMessage) -> Result<()> {
        self.tx.send(value).await?;

        Ok(())
    }

    pub async fn close(self) -> Result<()> {
        self.handle.await?;

        Ok(())
    }
}

pub(super) async fn spawn(
    folder: String,
    hashes: Vec<[u8; 20]>,
    files: Vec<TorrentFile>,
    picker: Arc<Picker>,
) -> Result<AssemblerHandle> {
    let (tx, rx) = mpsc::channel(20);

    let files = FileMap::from_files(files);

    Ok(AssemblerHandle {
        handle: task::spawn(
            async move {
                trace!("storage handler starting up");
                work(PathBuf::from(folder), hashes, files, picker, rx)
                    .instrument(trace_span!("work"))
                    .await;
                trace!("storage handler shutting down");
            }
            .in_current_span(),
        ),
        tx,
    })
}

#[derive(Debug)]
struct PartialPiece {
    filled: u32,
    blocks: BTreeMap<u32, Bytes>,
}

async fn work(
    folder: PathBuf,
    hashes: Vec<[u8; 20]>,
    files: FileMap,
    picker: Arc<Picker>,
    mut channel: mpsc::Receiver<StorageMessage>,
) {
    let mut partials: HashMap<u32, PartialPiece> = HashMap::new();
    let last_piece = (hashes.len() - 1) as u32;

    while let Some(message) = channel.recv().await {
        match message {
            StorageMessage::SubmitBlock { block, data } => {
                let piece_size = if block.index == last_piece {
                    let rem = files.len() % picker.piece_size as u64;

                    if rem == 0 {
                        picker.piece_size
                    } else {
                        rem as u32
                    }
                } else {
                    picker.piece_size
                };

                let partial = partials.entry(block.index).or_insert_with(|| {
                    trace!("new piece {} started", block.index);

                    PartialPiece {
                        filled: 0,
                        blocks: BTreeMap::new(),
                    }
                });

                partial.filled += data.len() as u32;
                partial.blocks.insert(block.begin, data);

                if partial.filled == piece_size {
                    let partial = partials.remove(&block.index).unwrap();
                    debug!("piece {} recieved. checking", block.index);
                    let piece = partial
                        .blocks
                        .into_iter()
                        .fold(
                            BytesMut::with_capacity(piece_size as usize),
                            |mut buf, (_, block)| {
                                buf.put(block);

                                buf
                            },
                        )
                        .freeze();

                    if validate(&hashes, block.index, &piece) {
                        let offset = (picker.piece_size as u64) * (block.index as u64);
                        match write(&folder, offset, piece_size, &files, block.index, piece).await {
                            Ok(()) => {
                                trace!(
                                    "write successful, removing partial piece {} from memory",
                                    block.index
                                );
                                continue;
                            }
                            Err(e) => {
                                error!("failed to write piece {} to disk: {}", block.index, e);
                            }
                        }
                    } else {
                        error!(
                            "piece {} failed hash check. returning to picker",
                            block.index
                        );
                    }

                    picker.add_piece(block.index);
                }
            }
            StorageMessage::Shutdown => {
                debug!("storage handler shutting down");
                return;
            }
        }
    }
}

fn validate(hashes: &[[u8; 20]], piece: u32, data: &Bytes) -> bool {
    trace!("checking the integrity of piece {}", piece);
    let mut hasher = Sha1::new();
    hasher.input(&data);

    let mut recieved_hash = [0; 20];
    hasher.result(&mut recieved_hash);

    recieved_hash == hashes[piece as usize]
}

async fn write(
    folder: &Path,
    offset: u64,
    piece_size: u32,
    files: &FileMap,
    piece: u32,
    mut data: Bytes,
) -> Result<()> {
    trace!("writing piece {} to disk", piece);
    let mut files = files.get_range(offset..offset + piece_size as u64)?.iter();

    if let Some(file) = files.next() {
        trace!("opening {:?}", file.path);
        let mut writer = OpenOptions::new()
            .write(true)
            .create(true)
            .open(&folder.join(&file.path))
            .await?;
        writer.set_len(file.len).await?;

        let start = offset - file.offset;
        trace!("seek to {}", start);
        writer.seek(SeekFrom::Start(start)).await?;

        let contents = data.split_to(usize::min((file.len - start) as usize, data.len()));
        writer.write_all(&contents).await?;

        trace!("{}", contents.len());
        for file in files {
            let mut writer = OpenOptions::new()
                .write(true)
                .create(true)
                .open(&folder.join(&file.path))
                .await?;
            writer.set_len(file.len).await?;

            let contents = data.split_to(usize::min(file.len as usize, data.len()));
            writer.write_all(&contents).await?;
        }
    }

    Ok(())
}
