mod manager;
mod node;
mod widget;

use std::sync::mpsc::Receiver;

pub use manager::UIManager;
pub use node::UINode;

use serde::Deserialize;

#[derive(Debug, Deserialize, PartialEq, Eq, Hash)]
pub struct UITag(u32);

#[derive(Debug, Deserialize)]
pub enum UIMessage {
    Test,
}

pub fn work(mut rx: Receiver<UIMessage>) {
    while let Ok(res) = rx.recv() {
        println!("{:#?}", res);
    }
}
