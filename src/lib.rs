#![doc(html_root_url = "https://docs.rs/lignin-dom/0.0.3")]
#![warn(clippy::pedantic)]

pub use lignin;

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

// pub mod diff;
pub mod load;
