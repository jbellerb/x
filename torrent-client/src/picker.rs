use std::fmt::{self, Display, Formatter};

use crate::protocol::{Bitfield, BLOCK_SIZE};

use crossbeam_queue::SegQueue;

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Block {
    pub index: u32,
    pub begin: u32,
    pub length: u32,
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "({}+{})", self.index, self.begin)
    }
}

#[derive(Debug)]
pub struct Picker {
    queue: SegQueue<Block>,
    pub piece_size: u32,
    pub size: u64,
    have: Bitfield,
}

impl Picker {
    pub fn new(piece_size: u32, pieces: u32, size: u64) -> Picker {
        Picker {
            queue: SegQueue::new(),
            piece_size,
            size,
            have: Bitfield::new(pieces as usize),
        }
    }

    pub fn push(&self, block: Block) {
        self.queue.push(block);
    }

    pub fn pop(&self) -> Option<Block> {
        if let Some(block) = self.queue.pop() {
            return Some(block);
        }

        None
    }

    pub fn add_piece(&self, piece: u32) {
        let piece_size = if piece == self.have.size as u32 - 1 {
            let rem = self.size % self.piece_size as u64;

            if rem == 0 {
                self.piece_size
            } else {
                rem as u32
            }
        } else {
            self.piece_size
        };

        let mut offset = 0;
        while offset < piece_size {
            let block_size = if offset + BLOCK_SIZE > piece_size {
                piece_size - offset
            } else {
                BLOCK_SIZE
            };

            self.queue.push(Block {
                index: piece,
                begin: offset,
                length: block_size,
            });
            offset += BLOCK_SIZE;
        }
    }

    pub fn pieces(&self) -> u32 {
        self.have.size as u32
    }

    pub fn left(&self) -> u64 {
        todo!("left")
    }
}
