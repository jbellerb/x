mod torrent;

use std::fs;
use torrent::Metainfo;

fn main() {
    let contents = fs::read("test.torrent").unwrap();

    let parsed_torrent = Metainfo::from_torrent(&contents).unwrap();

    println!("{:?}", parsed_torrent);
}
