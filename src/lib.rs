#![doc(html_root_url = "https://docs.rs/lignin-dom/0.0.3")]
#![warn(clippy::pedantic)]
#![allow(clippy::single_match_else)]

//! [lignin-dom](`self`) is a [***DOM***](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
//! loader and differ compatible with the [lignin] VDOM data structure library.
//!
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
//! ## RFC 2119 Blurb (modified stylization)
//!
//! The key words **must**, **must not**, **required**, **shall**, **shall
//! not**, **should**, **should not**, **recommended**,  **may**, and
//! **OPTIONAL** in this document are to be interpreted as described in
//! [RFC 2119](https://datatracker.ietf.org/doc/html/rfc2119).
//!
//! ## Web Terms
//!
//! Web terms are stylized ***bold italic*** and link to relevant documentation, usually on [MDN Web Docs](https://developer.mozilla.org/).
//!
//! As the terms [***DOM***](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
//! and [***HTML***](https://developer.mozilla.org/en-US/docs/Web/HTML)
//! appear very commonly, they are only linked in this location here and above.
//! In all other cases, the abbreviations should appear as normal text.
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
//! code that it interacts with indirectly through the DOM API.
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
//!
//! # Common Use Cases
//!
//! Items necessary for the following uses this crate are available as [re-exports](`self`#reexports).
//!
//! See the [modules](`self`#modules) for more specialized parts of the API.
//!
//! ## Hydration
//!
//! To hydrate an existing DOM structure, sent serialized as HTML from the server or otherwise,
//!
//! 1. use [`load_child_nodes`] to copy the existing children into a VDOM, then
//! 2. use it as `vdom_a` parameter to [`DomDiffer::update_child_nodes`].
//!
//! The loaded copy can be discarded immediately afterwards.
//!
//! The diff will be applied with relatively minimal changes, though
//!
//! - [`lignin::Node::Multi`],
//! - [`lignin::Node::Keyed`] and
//! - sequential [`lignin::Node::Text`] nodes (only when hydrating a DOM parsed from HTML)
//!
//! can currently throw this off within each child node list they appear in.
//!
//! A heuristic would be needed for more accurate hydration, which is compatible with [lignin]'s implementation contract
//! but likely won't appear in [lignin-dom](`self`) direction due to the complexity that would add here.
//!
//! > An approach that could work well with this existing renderer would be a loader that takes the target VDOM into account when creating the initial copy.
//! >
//! > This separate library would ideally be unloaded after the initial load, but [lignin] itself is not suitable for data sharing across assembly boundaries.
//! > Not only does it use [Rust-representation](https://doc.rust-lang.org/nomicon/repr-rust.html) of its data, which is not stable across compiles,
//! > but the VDOM data structure may also contain callback pointers relying on a single assembly-bound registry for memory safety.
//! >
//! > Still, the VDOM *information* (minus [callbacks](`lignin::CallbackRef`)) could be sent across such a boundary through lightweight serialization.
//!
//! ### Example
//!
//! TODO
//!
//! ## Invalidation/Update Loop
//!
//! Running a full diff is somewhat expensive, even if no changes are made to the DOM.
//!
//! As such, an app should
//!
//! - produce invalidation events when performing actions that will affect the GUI,
//! - collect and debounce these events throughout a JS event loop cycle and finally
//! - only if needed render to VDOM and diff against the previous version
//!
//! and otherwise stay passive.
//!
//! [lignin-dom](`self`) is only concerned with the final step of this process,
//! usually through the [`DomDiffer::update_child_nodes`] method.
//!
//! > And approach that works well is double-buffering the VDOM using an arena allocator like [bumpalo](https://lib.rs/crates/bumpalo),
//! but neither [lignin] nor [lignin-dom](`self`) are written in a way that would prefer a particular storage model beyond its general performance.
//!
//! ### Example
//!
//! TODO
//!
//! ## Starting from Scratch
//!
//! When rendering into an empty element, you can simply start with `&[]` as initial `vdom_a` parameter.
//!
//! > I recommend not doing this except for systems where you can control every aspect of execution and startup UX is of no concern.
//! >
//! > If you do not use server-side rendering, subjective performance of your app **will** be worse.
//! >
//! > If you discard an existing DOM and recreate it from scratch, this **will** lead to subtle UX glitches like unexpected loss of focus or entered user data in forms.
//! > It can also cause bad interactions with browser extensions a visitor may have installed and make debugging of your application unnecessarily more difficult.

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

mod rc_hash_map;
mod temp_set;

pub mod diff;
pub mod load;

pub use diff::DomDiffer;
pub use load::{load_child_nodes, Allocator};
