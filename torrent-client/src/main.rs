mod peer;
mod picker;
mod protocol;
mod swarm;
mod torrent;
mod tracker;

use std::fs;
use std::io;

use torrent::Metainfo;
use tracker::ping_tracker;

use anyhow::Result;
use tracing::{info_span, Level};
use tracing_futures::Instrument;
use tracing_subscriber::fmt::format::FmtSpan;

pub const PEER_PREFIX: &[u8; 8] = b"-M*010A-";

#[tokio::main]
async fn main() -> Result<()> {
    init_logging();

    let contents = fs::read("song.torrent")?;

    let parsed_torrent = Metainfo::from_bytes(&contents)?;

    //println!("{:?}", parsed_torrent);

    // let request = ping_tracker(&parsed_torrent).await?;

    // println!("{:#?}", request);

    let swarm = swarm::spawn(parsed_torrent)
        .instrument(info_span!("swarm"))
        .await?;

    swarm.close().await?;

    Ok(())
}

fn init_logging() {
    tracing_subscriber::fmt()
        .with_max_level(Level::DEBUG)
        .with_span_events(FmtSpan::NONE)
        .with_writer(io::stderr)
        .init();
}
