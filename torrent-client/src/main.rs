mod peer;
mod torrent;
mod tracker;

use std::fs;
use std::time::Duration;

use peer::Peer;
use torrent::Torrent;
use tracker::ping_tracker;

use anyhow::Result;
use tokio::net::lookup_host;
use tokio::time::timeout;

#[tokio::main]
async fn main() -> Result<()> {
    let contents = fs::read("test.torrent")?;

    let parsed_torrent = Torrent::from_torrent(&contents)?;

    //println!("{:?}", parsed_torrent);

    // let request = ping_tracker(&parsed_torrent).await?;

    // println!("{:#?}", request);
    for addr in lookup_host("localhost:39595").await? {
        let mut peer = timeout(
            Duration::from_secs(1),
            Peer::handshake(
                b"-M*0001-901234567890",
                &parsed_torrent.metainfo.infohash,
                addr,
            ),
        )
        .await??;

        println!("{:#?}", peer);

        peer.listen().await;
    }

    Ok(())
}
