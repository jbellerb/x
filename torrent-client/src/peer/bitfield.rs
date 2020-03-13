use bytes::BytesMut;

#[derive(Debug)]
pub struct Bitfield(pub BytesMut);

impl Bitfield {
    pub fn get(&mut self, block: usize) -> bool {
        let byte = block / 8;
        let bit = block % 8;

        self.0[byte] >> (7 - bit) & 1 != 0
    }

    pub fn set(&mut self, block: usize, state: bool) {
        let byte = block / 8;
        let bit = block % 8;

        if state {
            self.0[byte] |= 1 << (7 - bit)
        } else {
            self.0[byte] &= !(1 << (7 - bit))
        }
    }
}
