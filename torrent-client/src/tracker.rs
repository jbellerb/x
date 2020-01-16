use std::fmt;
use std::marker::PhantomData;
use std::str;

use super::torrent::Torrent;

use anyhow::{anyhow, Context, Result};
use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Serialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum Event {
    Started,
    Stopped,
    Completed,
}

#[derive(Serialize, Debug)]
struct TrackerRequest {
    #[serde(serialize_with = "serialize_bytes")]
    info_hash: [u8; 20],
    #[serde(serialize_with = "serialize_bytes")]
    peer_id: [u8; 20],
    port: u16,
    uploaded: u64,
    downloaded: u64,
    left: u64,
    compact: u8,
    event: Option<Event>,
    trackerid: Option<String>,
}

fn serialize_bytes<S>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    // I checked through the url 2.1.1 and serde_urlencoded 0.6.1 source, and
    // this shouldn't break anything. I should probably fix this later, though.
    unsafe { serializer.serialize_str(str::from_utf8_unchecked(bytes)) }
}

pub async fn ping_tracker(torrent: &Torrent) -> Result<TrackerResponse> {
    let tracker = &torrent.metainfo.announce;

    let form = TrackerRequest {
        info_hash: torrent.metainfo.infohash,
        peer_id: [1; 20],
        port: 1,
        uploaded: torrent.uploaded,
        downloaded: torrent.downloaded,
        left: torrent.left,
        compact: 1,
        event: None,
        trackerid: torrent.tracker_id.clone(),
    };

    let request = reqwest::Client::new()
        .get("https://httpbin.org/get")
        .query(&form)
        .send()
        .await.context(format!("Failed to connect to tracker {}.", tracker))?;

    let response = request.bytes().await?;

    println!("{:#?}", response);

    match serde_bencode::de::from_bytes(&response).context("Failed to parse tracker response.")? {
        response @ TrackerResponse::Success { .. } => Ok(response),
        TrackerResponse::Failure { failure_reason } => {
            Err(anyhow!("Tracker returned a failure: {}", failure_reason))
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum TrackerResponse {
    Failure {
        #[serde(rename = "failure reason")]
        failure_reason: String,
    },
    Success {
        interval: i32,
        #[serde(rename = "tracker id")]
        tracker_id: String,
        complete: i32,
        incomplete: i32,
        #[serde(deserialize_with = "peer_list")]
        peers: Vec<Peer>,
    },
}

#[derive(Serialize, Debug)]
pub struct Peer {
    peer_id: Option<String>,
    ip: String,
    port: i16,
}

pub fn peer_list<'de, D>(deserializer: D) -> Result<Vec<Peer>, D::Error>
where
    D: Deserializer<'de>,
{
    struct ChunkVisitor(PhantomData<fn() -> Peer>);

    impl<'de> Visitor<'de> for ChunkVisitor {
        type Value = Vec<Peer>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("bittorrent peer list in binary format")
        }

        fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut seq = v.iter();
            let mut peers = Vec::new();

            while let Some(value) = seq.next() {
                let mut chunk = [*value; 6];

                #[allow(clippy::needless_range_loop)]
                for i in 1..6 {
                    chunk[i] = *seq
                        .next()
                        .ok_or_else(|| de::Error::custom("invalid peer chunk"))?;
                }

                peers.push(Peer {
                    peer_id: None,
                    ip: String::from("Hi"),
                    port: 0,
                });
            }

            Ok(peers)
        }
    }

    let visitor = ChunkVisitor(PhantomData);
    deserializer.deserialize_any(visitor)
}
