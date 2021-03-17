#![doc(html_root_url = "https://docs.rs/lignin-dom/0.0.3")]
#![warn(clippy::pedantic)]
#![allow(clippy::single_match_else)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

mod rc_hash_map;

pub mod diff;
pub mod load;
