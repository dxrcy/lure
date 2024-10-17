use std::fs::{self, File};

use once_cell::sync::Lazy;

pub static mut OUT: Lazy<File> = Lazy::new(|| {
    //
    fs::OpenOptions::new()
        .write(true)
        .open("target/out.fifo")
        .unwrap()
});

macro_rules! print_out {
    ( $($tt:tt)* ) => {{
        let mut file = unsafe { &*OUT };
        write!(file, $($tt)*).expect("Failed to write to output file");
    }};
}
