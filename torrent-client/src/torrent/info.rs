use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum Info {
    SingleFile {
        #[serde(rename = "piece length")]
        piece_length: i64,
        #[serde(with = "chunk_hashes")]
        pieces: Vec<[u8; 20]>,
        name: String,
        length: i64,
    },
    MultiFile {
        #[serde(rename = "piece length")]
        piece_length: i64,
        #[serde(with = "chunk_hashes")]
        pieces: Vec<[u8; 20]>,
        name: String,
        files: Vec<TorrentFiles>,
    },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TorrentFiles {
    length: i64,
    path: Vec<String>,
}

mod chunk_hashes {
    use serde::de::{self, Visitor};
    use serde::{Deserializer, Serializer};

    use std::fmt;
    use std::marker::PhantomData;

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<[u8; 20]>, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ChunkVisitor(PhantomData<fn() -> u8>);

        impl<'de> Visitor<'de> for ChunkVisitor {
            type Value = Vec<[u8; 20]>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a nonempty array of 20 element chunks")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                let mut seq = v.iter();
                let mut chunks = Vec::new();

                while let Some(value) = seq.next() {
                    let mut chunk = [*value; 20];

                    #[allow(clippy::needless_range_loop)]
                    for i in 1..20 {
                        chunk[i] = *seq
                            .next()
                            .ok_or_else(|| de::Error::custom("invalid chunk of 20"))?;
                    }

                    chunks.push(chunk);
                }

                Ok(chunks)
            }
        }

        let visitor = ChunkVisitor(PhantomData);
        deserializer.deserialize_bytes(visitor)
    }

    pub fn serialize<S>(hashes: &[[u8; 20]], serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut output = Vec::new();

        for hash in hashes {
            output.extend(hash);
        }

        serializer.serialize_bytes(&output)
    }
}
