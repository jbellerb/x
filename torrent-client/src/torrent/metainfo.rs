use serde::{Deserialize, Serialize};
use serde_bencode::{de, error, ser};

use crypto::digest::Digest;
use crypto::sha1::Sha1;

use super::info::Info;

#[derive(Deserialize, Debug)]
struct RawMetainfo {
    info: Info,
    announce: String,
}

#[derive(Serialize, Debug)]
pub struct Metainfo {
    info: Info,
    #[serde(skip)]
    infohash: [u8; 20],
    announce: String,
}

impl Metainfo {
    pub fn from_torrent(torrent: &[u8]) -> Result<Metainfo, error::Error> {
        let parsed = de::from_bytes::<RawMetainfo>(torrent)?;

        let mut hasher = Sha1::new();
        hasher.input(&ser::to_bytes(&parsed.info)?);

        let mut infohash = [0; 20];
        hasher.result(&mut infohash);

        Ok(Metainfo {
            info: parsed.info,
            infohash,
            announce: parsed.announce,
        })
    }
}
