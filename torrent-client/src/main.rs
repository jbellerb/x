mod torrent;
mod tracker;

use serde_bencode::de;
use std::fs;
use torrent::Metainfo;
use tracker::TrackerResponse;

fn main() {
    let contents = fs::read("test.torrent").unwrap();

    let parsed_torrent = Metainfo::from_torrent(&contents).unwrap();

    //println!("{:?}", parsed_torrent);

    let test = "d14:failure reason23:some reason for failuree";

    let parsed_response = de::from_str::<TrackerResponse>(test).unwrap();

    println!("{:#?}", parsed_response);
}
