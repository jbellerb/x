mod torrent;
use std::fs;
use torrent::metainfo::Metainfo;

fn main() {
    let contents = fs::read("test.torrent").unwrap();

    let parsed_torrent = Metainfo::from_torrent(&contents).unwrap();

    println!("{}", serde_json::to_string(&parsed_torrent).unwrap());
}
