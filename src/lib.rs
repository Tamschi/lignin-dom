#![doc(html_root_url = "https://docs.rs/lignin-dom/0.0.3")]
#![warn(clippy::pedantic)]
#![allow(clippy::single_match_else)]

//! # About this Documentation
//!
//! ## Comments
//!
//! Non-normative comments appear in this documentation
//!
//! > like this
//!
//! and contain further explanations, motivations or just general reminders in slightly more liberal prose.
//!
//! I recommend reading them, but you can always skip them without missing out on information needed to correctly use this library.
//!
//! ## Web Terms
//!
//! Web terms are stylized ***bold italic*** and link to relevant documentation, usually on [MDN Web Docs](https://developer.mozilla.org/).
//!
//! # Tracing
//!
//! [lignin-dom](`self`) uses [tracing] as tracing and logging framework.
//!
//! Use [tracing-wasm](https://crates.io/crates/tracing-wasm) to get clean traces in a browser's developer tools.
//!
//! Use [tracing]'s [compile time level filters](`tracing::level_filters`#compile-time-filters)
//! to optimize debug code paths out of your binary.  
//! See [tracing]'s documentation for information on dynamic filters and event processing.
//!
//! <!-- TODO: Make it unlikely to be a legal issue to transmit `lignin_dom` error logs back to the server, then mention that here. -->
//!
//! # Strictness
//!
//! [lignin-dom](`self`) is strict in its Rust API but very lenient towards [***JavaScript***](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
//! code that it interacts with indirectly through the [***DOM***](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model) API.
//!
//! This comes with a few guarantees:
//!
//! - Unexpected DOM state will not lead to
//!   - errors or panics *on the Rust side*,
//!   - (exacerbation of) memory leaks,
//!   - access violations or other memory mismanagement.
//! - Unexpected DOM state does not stop *changed* parts of the VDOM from being updated into the DOM tree.
//!   > However, parts of the document that appear unchanged in the VDOM diff **may** retain values edited in the DOM through the diff being applied. This is explicitly unspecified!
//!
//! In addition to this, [lignin-dom](`self`) obey's [lignin's implementation contract](lignin#implementation-contract).
//! It won't touch siblings past those mentioned in the VDOM or attributes that do not appear in it.
//! 
//! > [***Attr***](https://developer.mozilla.org/en-US/docs/Web/API/Attr)ibutes that are to be added but unexpectedly already exist are overwritten while logging an [error](`tracing::error`).

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

mod rc_hash_map;
mod temp_set;

pub mod diff;
pub mod load;
