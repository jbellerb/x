mod torrent;
mod tracker;

use std::fs;

use torrent::Torrent;
use tracker::ping_tracker;

use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    let contents = fs::read("test.torrent")?;

    let parsed_torrent = Torrent::from_torrent(&contents)?;

    //println!("{:?}", parsed_torrent);

    let request = ping_tracker(&parsed_torrent).await?;

    println!("{:#?}", request);

    Ok(())
}
