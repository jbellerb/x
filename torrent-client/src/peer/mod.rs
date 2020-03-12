use serde::Serialize;

use anyhow::Result;

#[derive(Serialize, Debug)]
pub struct Peer {
    pub peer_id: String,
    pub address: String,
    pub peer_choke: bool,
    pub self_choke: bool,
    pub peer_interest: bool,
    pub self_interest: bool,
}

impl Peer {
    pub async fn handshake(peer_id: String, address: String) -> Result<Peer> {
        unimplemented!()
    }
}
