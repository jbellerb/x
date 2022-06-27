mod bitfield;
mod codec;

pub use bitfield::Bitfield;
pub(crate) use codec::{Codec, Frame};

pub const BLOCK_SIZE: u32 = 16384;
pub const MAX_PENDING: usize = 10;
