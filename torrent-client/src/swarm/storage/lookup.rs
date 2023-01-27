use std::ops::Range;
use std::path::PathBuf;

use crate::torrent::TorrentFile;

use anyhow::Result;

#[derive(Debug)]
pub struct File {
    pub path: PathBuf,
    pub offset: u64,
    pub len: u64,
}

pub struct FileMap {
    files: Vec<File>,
    len: u64,
}

impl FileMap {
    pub fn from_files(files: Vec<TorrentFile>) -> FileMap {
        let mut offset = 0;

        FileMap {
            files: {
                files
                    .into_iter()
                    .map(|file| {
                        let file = File {
                            path: file.path,
                            offset,
                            len: file.length,
                        };

                        offset += file.len;

                        file
                    })
                    .collect()
            },
            len: offset,
        }
    }

    pub fn get_range(&self, range: Range<u64>) -> Result<&[File]> {
        let start = match self
            .files
            .binary_search_by(|file| file.offset.cmp(&range.start))
        {
            Ok(a) => a,
            Err(a) => a - 1,
        };

        let end = match self
            .files
            .binary_search_by(|file| file.offset.cmp(&range.end))
        {
            Ok(a) => a - 1,
            Err(a) => a,
        };

        Ok(&self.files[start..end])
    }

    pub fn len(&self) -> u64 {
        self.len
    }
}
