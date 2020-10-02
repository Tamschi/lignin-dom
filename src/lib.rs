#![doc(html_root_url = "https://docs.rs/lignin-dom/0.0.1")]
#![warn(clippy::pedantic)]

pub use lignin;

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

mod closure_map;
pub mod diff;
pub mod load;
