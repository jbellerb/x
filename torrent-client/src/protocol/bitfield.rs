use bytes::BytesMut;

#[derive(Debug)]
pub struct Bitfield {
    bytes: BytesMut,
    pub size: usize,
}

impl Bitfield {
    pub fn new(size: usize) -> Bitfield {
        let capacity = (size + 7) / 8;
        let mut bytes = BytesMut::with_capacity(capacity);
        bytes.resize(capacity, 0);

        Bitfield { bytes, size }
    }

    pub fn from_bytes(bytes: BytesMut, size: usize) -> Bitfield {
        Bitfield { bytes, size }
    }

    pub fn get(&self, block: usize) -> bool {
        let byte = block / 8;
        let bit = block % 8;

        self.bytes[byte] >> (7 - bit) & 1 != 0
    }

    pub fn set(&mut self, block: usize, state: bool) {
        let byte = block / 8;
        let bit = block % 8;

        if state {
            self.bytes[byte] |= 1 << (7 - bit)
        } else {
            self.bytes[byte] &= !(1 << (7 - bit))
        }
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    fn count(&self) -> usize {
        let mut count = 0;

        for byte in &self.bytes {
            count += byte.count_ones() as usize;
        }

        count
    }

    pub fn progress(&self) -> f64 {
        self.count() as f64 / self.size as f64
    }
}
