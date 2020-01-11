use serde::de::{self, Visitor};
use serde::{Deserialize, Deserializer, Serialize};

use std::fmt;
use std::marker::PhantomData;

#[derive(Serialize, Debug)]
pub enum Event {
    Started,
    Stopped,
    Completed
}

#[derive(Serialize, Debug)]
pub struct TrackerRequest {
    info_hash: [u8; 20],
    peer_id: [u8; 20],
    port: u16,
    uploaded: u64,
    downloaded: u64,
    left: u64,
    event: Option<Event>,
    trackerid: Option<String>,
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
