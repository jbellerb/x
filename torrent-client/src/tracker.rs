use std::fmt;
use std::marker::PhantomData;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::str;

use crate::swarm::SwarmHandle;

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

pub async fn ping_tracker(swarm: &SwarmHandle) -> Result<TrackerResponse> {
    let form = TrackerRequest {
        info_hash: swarm.infohash,
        peer_id: [1; 20],
        port: 1,
        uploaded: swarm.uploaded(),
        downloaded: swarm.downloaded(),
        left: swarm.left(),
        compact: 1,
        event: None,
        trackerid: swarm.tracker_id.clone(),
    };

    let request = reqwest::Client::new()
        .get(&swarm.announce)
        .query(&form)
        .send()
        .await
        .context(format!("Failed to connect to tracker {}.", &swarm.announce))?;

    let response = request.bytes().await?;

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
        #[serde(deserialize_with = "peer_compact")]
        peers: Vec<SocketAddr>,
    },
}

pub fn peer_compact<'de, D>(deserializer: D) -> Result<Vec<SocketAddr>, D::Error>
where
    D: Deserializer<'de>,
{
    struct ChunkVisitor(PhantomData<fn() -> SocketAddr>);

    impl<'de> Visitor<'de> for ChunkVisitor {
        type Value = Vec<SocketAddr>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("bittorrent peer list in binary format")
        }

        fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut chunks = v.chunks_exact(6);
            let mut peers = Vec::new();

            while let Some(chunk) = chunks.next() {
                peers.push(SocketAddr::new(
                    IpAddr::V4(Ipv4Addr::new(chunk[0], chunk[1], chunk[2], chunk[3])),
                    u16::from_be_bytes([chunk[4], chunk[5]]),
                ));
            }

            if !chunks.remainder().is_empty() {
                return Err(de::Error::custom("invalid peer chunk"));
            }

            Ok(peers)
        }
    }

    let visitor = ChunkVisitor(PhantomData);
    deserializer.deserialize_any(visitor)
}
