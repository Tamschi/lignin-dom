use crate::{
	rc_hash_map::{self, RcHashMap},
	temp_set::TempEventBindingSet,
};
use core::{any::type_name, convert::TryInto, iter, slice};
use hashbrown::{hash_map::Entry, HashSet};
use js_sys::Function;
use lignin::{callback_registry::CallbackSignature, CallbackRef, DomRef, EventBinding, ReorderableFragment, ThreadBound};
use tracing::{error, info, instrument, level_filters::STATIC_MAX_LEVEL, trace, trace_span, warn, Level};
use wasm_bindgen::{closure::Closure, throw_str, JsCast, JsValue, UnwrapThrowExt};

/// Attached to a specific [`web_sys::Element`] during instantiation, this `struct` can be used to update its [***childNodes***](https://developer.mozilla.org/en-US/docs/Web/API/Node/childNodes).
///
/// Note that this does not include the [***Element***](https://developer.mozilla.org/en-US/docs/Web/API/element)
/// it is attached to itself, and also does not include the [***Attr***](https://developer.mozilla.org/en-US/docs/Web/API/Attr)ibutes of that element.
///
/// # Safety
///
/// Event handlers are reference-counted per [`DomDiffer`] instance and memory-safe, including interactions with misbehaving [***JavaScript***](https://developer.mozilla.org/en-US/docs/Web/JavaScript) code.
///
/// However, associated event listeners will start throwing errors into [***JavaScript***](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
/// if the instance is dropped.
///
/// They may also do so if the [`DomDiffer`] is [misused](`DomDiffer`#correct-use) in a way that causes handles to be dropped early.
#[allow(clippy::type_complexity)]
#[derive(Debug)]
pub struct DomDiffer {
	handler_handles: RcHashMap<CallbackRef<ThreadBound, fn(lignin::web::Event)>, u16, Function>,
	common_handler: Closure<dyn Fn(JsValue, web_sys::Event)>,
	element: web_sys::Element,
	event_listener_options_cache: [Option<web_sys::AddEventListenerOptions>; 8],
	event_binding_diff_set: TempEventBindingSet,
}
impl DomDiffer {
	#[must_use]
	#[instrument]
	pub fn new_for_element_child_nodes(element: web_sys::Element) -> Self {
		Self {
			handler_handles: RcHashMap::new(),
			common_handler: Closure::wrap(Box::new(move |callback_ref: JsValue, event: web_sys::Event| {
				let span = trace_span!("common_handler", callback_ref = ?&callback_ref, event = ?&event);
				let _enter = span.enter();

				let callback_ref = unsafe { CallbackRef::<ThreadBound, fn(lignin::web::Event)>::from_js(&callback_ref) };
				let callback_ref = if cfg!(debug_assertions) {
					callback_ref.unwrap_or_else(move || panic!("lignin-dom bug: Invalid `CallbackRef` {:?}", callback_ref))
				} else {
					callback_ref.expect_throw("lignin-dom bug: Invalid `CallbackRef`. Compile with debug assertions to see the value.")
				};

				#[allow(clippy::non_ascii_literal)]
				let span = trace_span!("callback_ref.call(…)");
				let _enter = span.enter();
				callback_ref.call(event.into());
			})),
			element,
			event_listener_options_cache: [None, None, None, None, None, None, None, None],
			event_binding_diff_set: TempEventBindingSet::new(),
		}
	}

	#[allow(clippy::type_complexity)]
	#[instrument]
	fn get_or_create_listener<'a>(
		common_handler: &Closure<dyn Fn(JsValue, web_sys::Event)>,
		handler_handles: &'a mut RcHashMap<CallbackRef<ThreadBound, fn(lignin::web::Event)>, u16, Function>,
		callback_ref: CallbackRef<ThreadBound, fn(lignin::web::Event)>,
	) -> &'a Function {
		handler_handles
			.increment_or_insert_with(callback_ref, |callback_ref| common_handler.as_ref().unchecked_ref::<Function>().bind1(&JsValue::UNDEFINED, &callback_ref.into_js()))
			.expect_throw("Too many (more than 65k) active references to the same `CallbackRef`")
	}

	#[instrument]
	fn get_cached_add_event_listener_options(event_listener_options_cache: &mut [Option<web_sys::AddEventListenerOptions>; 8], options: lignin::EventBindingOptions) -> &web_sys::AddEventListenerOptions {
		let entry = event_listener_options_cache
			.get_mut(options.capture() as usize + options.once() as usize * 2 + options.passive() as usize * 4)
			.unwrap_throw();

		if entry.is_none() {
			let mut web_options = web_sys::AddEventListenerOptions::new();
			web_options.capture(options.capture()).once(options.once()).passive(options.passive());
			*entry = Some(web_options)
		}

		entry.as_ref().unwrap_throw()
	}

	/// # Correct Use
	#[instrument(skip(vdom_a, vdom_b))]
	pub fn update_child_nodes(&mut self, vdom_a: &[lignin::Node<'_, ThreadBound>], vdom_b: &[lignin::Node<'_, ThreadBound>], depth_limit: usize) {
		let element = self.element.clone();
		let child_nodes = self.element.child_nodes();
		let owner_document = self.element.owner_document().expect_throw("lignin-dom: No owner document found for root element.");
		let mut i = 0;
		self.diff_splice_node_list(&owner_document, vdom_a, vdom_b, &element, &child_nodes, &mut i, depth_limit);
		debug_assert_eq!(i, lignin::Node::Multi(vdom_b).dom_len().try_into().unwrap_throw());

		{
			let drain = self.handler_handles.drain_weak();
			trace!("Freed {} event listener(s).", drain.count());
		}
		info!("Event listener count/cached capacity: {}/{}", self.handler_handles.len(), self.handler_handles.capacity());
		info!("Diff heap capacity (event bindings): {}", self.event_binding_diff_set.capacity());
		if STATIC_MAX_LEVEL >= Level::WARN && self.event_binding_diff_set.capacity() >= 100 {
			warn!(
				"The event binding diff heap capacity is large ({}).\n\
				This may point to inefficient (unstably ordered) use of event bindings.",
				self.event_binding_diff_set.capacity()
			)
		}
		//TODO: Log/warn heap cache metrics.
	}

	/// If `vdom_b` is empty, `next_sibling` is guaranteed unused.
	#[allow(clippy::too_many_arguments)]
	#[allow(clippy::too_many_lines)]
	#[instrument(skip(vdom_a, vdom_b))]
	fn diff_splice_node_list(
		&mut self,
		document: &web_sys::Document,
		mut vdom_a: &[lignin::Node<'_, ThreadBound>],
		mut vdom_b: &[lignin::Node<'_, ThreadBound>],
		parent_element: &web_sys::Element,
		dom_slice: &web_sys::NodeList,
		i: &mut u32,
		depth_limit: usize,
	) {
		if depth_limit == 0 {
			return error!("Depth limit reached");
		}

		#[allow(clippy::never_loop)] // Inner `loop`.
		while !vdom_a.is_empty() && !vdom_b.is_empty() {
			*i += 'vdom_item: loop {
				break 'vdom_item match (vdom_a[0], vdom_b[0]) {
					(lignin::Node::Comment { comment: c_1, dom_binding: db_1 }, lignin::Node::Comment { comment: c_2, dom_binding: db_2 }) => {
						let span = trace_span!("Diffing comment", c_1, c_2, ?db_1, ?db_2);
						let _enter = span.enter();
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected comment beyond end of `web_sys::NodeList`. Switching to insertions.");
								self.discard_further_missing(0, vdom_a.iter(), depth_limit);
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, depth_limit);
							}
						};

						let comment = match node.dyn_ref::<web_sys::Comment>() {
							Some(comment) => comment,
							None => {
								error!("Expected to update `web_sys::Comment` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::Comment { comment: c_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::Comment { comment: c_2, dom_binding: db_2 }], parent_element, dom_slice, i, depth_limit);
								break 'vdom_item 0;
							}
						};

						let _guard = loosen_binding(db_1, db_2, comment.into());
						if STATIC_MAX_LEVEL >= Level::ERROR && comment.data() != c_1 {
							if c_1 == c_2 {
								error!("Unexpected comment data that won't be updated: Expected {:?} but found {:?}", c_1, comment.data());
							} else {
								warn!("Unexpected comment data: Expected {:?} but found {:?}", c_1, comment.data());
							}
						}
						if c_1 != c_2 {
							comment.set_data(c_2)
						}
						1
					}

					(lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }, lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }) if e_1.name == e_2.name && e_1.creation_options == e_2.creation_options => {
						let span = trace_span!("Diffing HTML element", tag = e_1.name, ?db_1, ?db_2);
						let _enter = span.enter();
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								self.discard_further_missing(0, vdom_a.iter(), depth_limit);
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, depth_limit);
							}
						};

						let html_element = match node.dyn_ref::<web_sys::HtmlElement>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::HtmlElement` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }], parent_element, dom_slice, i, depth_limit);
								break 'vdom_item 0;
							}
						};

						if html_element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the HTML element.", e_1.name, html_element.tag_name());
							self.diff_splice_node_list(document, &[lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
							self.diff_splice_node_list(document, &[], &[lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }], parent_element, dom_slice, &mut *i, depth_limit);
						}

						let _guard = loosen_binding(db_1, db_2, html_element.into());

						self.update_element(document, e_1, e_2, html_element, depth_limit);
						1
					}

					(lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }, lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }) if e_1.name == e_2.name && e_1.creation_options == e_2.creation_options => {
						let span = trace_span!("Diffing MathML element", tag = e_1.name, ?db_1, ?db_2);
						let _enter = span.enter();
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								self.discard_further_missing(0, vdom_a.iter(), depth_limit);
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, depth_limit);
							}
						};

						let element = match node.dyn_ref::<web_sys::Element>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::Element` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }], parent_element, dom_slice, i, depth_limit);
								break 'vdom_item 0;
							}
						};

						if element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the MathML element.", e_1.name, element.tag_name());
							self.diff_splice_node_list(document, &[lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
							self.diff_splice_node_list(document, &[], &[lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }], parent_element, dom_slice, &mut *i, depth_limit);
						}

						let _guard = loosen_binding(db_1, db_2, element.into());

						self.update_element(document, e_1, e_2, element, depth_limit);
						1
					}

					(lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }, lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }) if e_1.name == e_2.name && e_1.creation_options == e_2.creation_options => {
						let span = trace_span!("Diffing SVG element", tag = e_1.name, ?db_1, ?db_2);
						let _enter = span.enter();
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								self.discard_further_missing(0, vdom_a.iter(), depth_limit);
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, depth_limit);
							}
						};

						let svg_element = match node.dyn_ref::<web_sys::SvgElement>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::SvgElement` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }], parent_element, dom_slice, i, depth_limit);
								break 'vdom_item 0;
							}
						};

						if svg_element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the SVG element.", e_1.name, svg_element.tag_name());
							self.diff_splice_node_list(document, &[lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
							self.diff_splice_node_list(document, &[], &[lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }], parent_element, dom_slice, &mut *i, depth_limit);
						}

						let _guard = loosen_binding(db_1, db_2, svg_element.into());

						self.update_element(document, e_1, e_2, svg_element, depth_limit);
						1
					}

					(lignin::Node::Memoized { state_key: sk_1, content: c_1 }, lignin::Node::Memoized { state_key: sk_2, content: c_2 }) => {
						let span = trace_span!("Diffing memoized", sk_1, sk_2);
						let _enter = span.enter();
						if sk_1 == sk_2 {
							//TODO: Recursion limit.
							let dom_len: u32 = c_2.dom_len().try_into().unwrap_throw();
							trace!("State keys matched. Advancing past {} DOM node(s).", dom_len);
							dom_len
						} else {
							let span = trace_span!("State keys mismatched", sk_1, sk_2);
							let _enter = span.enter();
							self.diff_splice_node_list(document, slice::from_ref(c_1), slice::from_ref(c_2), parent_element, dom_slice, i, depth_limit - 1);
							0
						}
					}

					(lignin::Node::Multi(n_1), lignin::Node::Multi(n_2)) => {
						let span = trace_span!("Diffing multi", "n_1.len()" = n_1.len(), "n_2.len()" = n_2.len());
						let _enter = span.enter();
						// Skip `depth_limit` check one level down if there are no items at all.
						if !n_1.is_empty() || !n_2.is_empty() {
							self.diff_splice_node_list(document, n_1, n_2, parent_element, dom_slice, i, depth_limit - 1)
						}
						0
					}

					(lignin::Node::Keyed(mut rf_1), lignin::Node::Keyed(mut rf_2)) => {
						let span = trace_span!("Diffing keyed", "rf_1.len()" = rf_1.len(), "rf_2.len()" = rf_2.len());
						let _enter = span.enter();

						debug_assert_eq!(rf_2.len(), rf_2.iter().map(|b| b.dom_key).collect::<HashSet<_>>().len(), "Duplicate `ReorderableFragment::key` encountered");

						while !rf_1.is_empty() {
							let &ReorderableFragment { dom_key: dk_1, content: ref c_1 } = rf_1.get(0).unwrap_throw();

							if let Some(&ReorderableFragment { dom_key: dk_2, content: ref c_2 }) = rf_2.first() {
								if dk_1 == dk_2 {
									self.diff_splice_node_list(document, slice::from_ref(c_1), slice::from_ref(c_2), parent_element, dom_slice, i, depth_limit - 1);
									rf_1 = &rf_1[1..];
									rf_2 = &rf_2[1..];
									continue;
								}
							}

							break;
						}

						if rf_1.is_empty() {
							for ReorderableFragment { dom_key: _, content } in rf_2 {
								self.diff_splice_node_list(document, &[], slice::from_ref(content), parent_element, dom_slice, i, depth_limit - 1)
							}
						} else if rf_2.is_empty() {
							for ReorderableFragment { dom_key: _, content } in rf_1 {
								self.diff_splice_node_list(document, slice::from_ref(content), &[], parent_element, dom_slice, i, depth_limit - 1)
							}
						} else {
							// Help wanted: This is an inefficient algorithm.
							//TODO: Test this thoroughly!
							let mut map = hashbrown::HashMap::<_, (&lignin::Node<ThreadBound>, Option<Vec<web_sys::Node>>)>::new();

							// Create target slot index:
							for b in rf_2 {
								match map.entry(b.dom_key) {
									Entry::Occupied(_) => {
										if cfg!(debug_assertions) {
											panic!("Duplicate `ReorderableFragment::dom_key` encountered: {}", b.dom_key);
										} else {
											throw_str("Duplicate `ReorderableFragment::dom_key` encountered.")
										}
									}
									Entry::Vacant(v) => {
										v.insert((&b.content, None));
									}
								}
							}

							// Diff and collect target DOM nodes:
							for a in rf_1 {
								let slot = map.get_mut(&a.dom_key);
								match slot {
									None => self.diff_splice_node_list(document, slice::from_ref(&a.content), &[], parent_element, dom_slice, i, depth_limit - 1),
									Some(slot) => {
										let mut k = *i;
										self.diff_splice_node_list(document, slice::from_ref(&a.content), slice::from_ref(slot.0), parent_element, dom_slice, &mut k, depth_limit - 1);
										slot.1 = Some(
											iter::repeat_with(|| -> web_sys::Node { parent_element.remove_child(&dom_slice.get(*i).unwrap_throw()).unwrap_throw() })
												.take((k - *i).try_into().unwrap_throw())
												.collect(),
										);
									}
								}
							}

							// Reinsert DOM nodes in order and diff-create new ones where necessary:
							let next_sibling = dom_slice.get(*i);
							let next_sibling = next_sibling.as_ref();
							for b in rf_2 {
								let slot = map.remove(&b.dom_key).unwrap_throw();
								debug_assert_eq!(&b.content as *const _, slot.0 as *const _);
								match slot.1 {
									None => self.diff_splice_node_list(document, &[], slice::from_ref(&b.content), parent_element, dom_slice, i, depth_limit - 1),
									Some(nodes) => {
										for node in nodes {
											match parent_element.insert_before(&node, next_sibling) {
												Ok(_) => *i += 1,
												Err(error) => error!("Failed to reinsert node: {:?}", error),
											}
										}
									}
								}
							}
							debug_assert!(map.is_empty());
						}
						0
					}

					(lignin::Node::Text { text: t_1, dom_binding: db_1 }, lignin::Node::Text { text: t_2, dom_binding: db_2 }) => {
						let span = trace_span!("Diffing text node", t_1, t_2, ?db_1, ?db_2);
						let _enter = span.enter();
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected text beyond end of `web_sys::NodeList`. Switching to insertions.");
								self.discard_further_missing(0, vdom_a.iter(), depth_limit);
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, depth_limit);
							}
						};

						let text = match node.dyn_ref::<web_sys::Text>() {
							Some(comment) => comment,
							None => {
								error!("Expected to update `web_sys::Text` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::Text { text: t_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::Text { text: t_2, dom_binding: db_2 }], parent_element, dom_slice, i, depth_limit);
								break 'vdom_item 0;
							}
						};

						let _guard = loosen_binding(db_1, db_2, text.into());
						if text.data() != t_1 {
							error!("Unexpected text data: Expected {:?} but found {:?}. Overwriting.", t_1, text.data());
							text.set_data(t_2)
						} else if t_1 != t_2 {
							text.set_data(t_2)
						}
						1
					}

					(lignin::Node::RemnantSite(_), lignin::Node::RemnantSite(_)) => {
						let span = trace_span!("Diffing remnant site");
						let _enter = span.enter();
						todo!("`RemnantSite` diff")
					}

					// Mismatching nodes: Destroy and rebuild.
					(ref n_1, ref n_2) => {
						let span = trace_span!("Replace mismatching");
						let _enter = span.enter();

						if STATIC_MAX_LEVEL >= Level::WARN {
							if let (&lignin::Node::HtmlElement { element: e_1, .. }, &lignin::Node::HtmlElement { element: e_2, .. })
							| (&lignin::Node::SvgElement { element: e_1, .. }, &lignin::Node::SvgElement { element: e_2, .. }) = (n_1, n_2)
							{
								if e_1.name.eq_ignore_ascii_case(e_2.name) {
									warn!(
										"Recreating element due to different tag name casing: {:?} -> {:?}\n\
										This is a `cfg!(debug_assertions)`-only warning, but the performance impact will persist in production.",
										e_1.name, e_2.name
									)
								}
							}
						}

						self.diff_splice_node_list(document, slice::from_ref(n_1), &[], parent_element, dom_slice, i, depth_limit);
						self.diff_splice_node_list(document, &[], slice::from_ref(n_2), parent_element, dom_slice, i, depth_limit);
						0
					}
				};
			};

			vdom_a = &vdom_a[1..];
			vdom_b = &vdom_b[1..];
		}

		let mut vdom_a = vdom_a.iter();
		for removed_node in vdom_a.by_ref() {
			let handler_decrement_count = self.decrement_handlers(removed_node, depth_limit);

			macro_rules! remove_element {
				($element:expr, $dom_binding:expr, $debug_kind:literal, $web_type:ty) => {{
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!(
								"Expected to remove {} element beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.",
								$debug_kind
							);
							self.discard_further_missing(handler_decrement_count, vdom_a, depth_limit);
							break;
						}
					};

					let dom_element = match node.dyn_ref::<$web_type>() {
						Some(dom_element) => dom_element,
						None => {
							error!(
								"Expected to remove `{}` but found {:?}; (Inefficiently) deleting the node anyway but ignoring bindings.",
								type_name::<$web_type>(),
								node
							);

							match node.parent_node() {
								Some(parent) => {
									if let Err(error) = parent.remove_child(&node) {
										error!("Failed to remove the node: {:?}", error)
									}
								}
								None => error!("Could not find parent node of node to remove. Ignoring."),
							}
							continue;
						}
					};

					if dom_element.tag_name() != $element.name {
						error!("Expected to remove <{}> but found <{}>; Removing anyway but ignoring bindings.", $element.name, dom_element.tag_name());
						if handler_decrement_count != 0 {
							warn!("{} removed event bindings were unaccounted for.", handler_decrement_count)
						}
						dom_element.remove();
						continue;
					}

					if let Some(dom_binding) = $dom_binding {
						dom_binding.call(DomRef::Removing(dom_element.into()))
					}

					self.unbind_node(document, &$element.content, &dom_element.child_nodes(), &mut 0, depth_limit - 1);

					dom_element.remove()
				};};
			}

			match *removed_node {
				lignin::Node::Comment { comment, dom_binding } => {
					let span = trace_span!("Removing comment", comment, ?dom_binding);
					let _enter = span.enter();
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to remove comment beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.");
							self.discard_further_missing(handler_decrement_count, vdom_a, depth_limit);
							break;
						}
					};

					let dom_comment = match node.dyn_ref::<web_sys::Comment>() {
						Some(comment) => comment,
						None => {
							error!("Expected to remove `web_sys::Comment` but found {:?}; (Inefficiently) deleting the node anyway but ignoring bindings.", node);

							match node.parent_node() {
								Some(parent) => {
									if let Err(error) = parent.remove_child(&node) {
										error!("Failed to remove the node: {:?}", error)
									}
								}
								None => error!("Could not find parent node of node to remove. Ignoring."),
							}
							continue;
						}
					};

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Removing(dom_comment.into()))
					}

					if STATIC_MAX_LEVEL >= Level::ERROR && dom_comment.data() != comment {
						error!("Unexpected removed comment data: {:?}", dom_comment.data())
					}

					dom_comment.remove();
				}

				lignin::Node::HtmlElement { element, dom_binding } => {
					let span = trace_span!("Removing HTML element", tag = element.name, ?dom_binding);
					let _enter = span.enter();
					remove_element!(element, dom_binding, "HTML", web_sys::HtmlElement)
				}

				lignin::Node::MathMlElement { element, dom_binding } => {
					let span = trace_span!("Removing MathML element", tag = element.name, ?dom_binding);
					let _enter = span.enter();
					remove_element!(element, dom_binding, "MathML", web_sys::Element)
				}

				lignin::Node::SvgElement { element, dom_binding } => {
					let span = trace_span!("Removing SVG element", tag = element.name, ?dom_binding);
					let _enter = span.enter();
					remove_element!(element, dom_binding, "SVG", web_sys::SvgElement)
				}

				lignin::Node::Memoized { state_key, content } => {
					let span = trace_span!("Removing memoized", state_key);
					let _enter = span.enter();
					self.diff_splice_node_list(document, slice::from_ref(content), &[], parent_element, dom_slice, i, depth_limit - 1)
				}

				lignin::Node::Multi(nodes) => {
					let span = trace_span!("Removing multi", "nodes.len()" = nodes.len());
					let _enter = span.enter();
					// May skip `depth_limit` check one level down.
					if !nodes.is_empty() {
						self.diff_splice_node_list(document, nodes, &[], parent_element, dom_slice, i, depth_limit - 1);
					}
				}

				lignin::Node::Keyed(reorderable_fragments) => {
					let _enter = trace_span!("Removing keyed", "reorderable_fragments.len()" = reorderable_fragments.len());
					for reorderable_fragment in reorderable_fragments {
						let _enter = trace_span!("Removing keyed fragment", dom_key = reorderable_fragment.dom_key);
						self.diff_splice_node_list(document, slice::from_ref(&reorderable_fragment.content), &[], parent_element, dom_slice, i, depth_limit - 1)
					}
				}

				lignin::Node::Text { text, dom_binding } => {
					let span = trace_span!("Removing text node", text, ?dom_binding);
					let _enter = span.enter();
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to remove text beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.");
							self.discard_further_missing(handler_decrement_count, vdom_a, depth_limit);
							break;
						}
					};

					let dom_text = match node.dyn_ref::<web_sys::Text>() {
						Some(text) => text,
						None => {
							error!("Expected to remove `web_sys::Text` but found {:?}; (Inefficiently) deleting the node anyway but ignoring bindings.", node);

							match node.parent_node() {
								Some(parent) => {
									if let Err(error) = parent.remove_child(&node) {
										error!("Failed to remove the node: {:?}", error)
									}
								}
								None => error!("Could not find parent node of node to remove. Ignoring."),
							}
							continue;
						}
					};

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Removing(dom_text.into()))
					}

					if STATIC_MAX_LEVEL >= Level::ERROR && dom_text.data() != text {
						error!("Unexpected removed text data: {:?}", dom_text.data())
					}

					dom_text.remove();
				}

				lignin::Node::RemnantSite(_) => {
					let span = trace_span!("Removing remnant site");
					let _enter = span.enter();
					todo!("Remove `RemnantSite`")
				}
			}
		}

		let next_sibling = dom_slice.get(*i);
		let next_sibling = next_sibling.as_ref();
		for new_node in vdom_b {
			*i += match *new_node {
				lignin::Node::Comment { comment, dom_binding } => {
					let span = trace_span!("Creating comment", comment, ?dom_binding);
					let _enter = span.enter();
					let dom_comment = document.create_comment(comment);
					if let Err(error) = parent_element.insert_before(dom_comment.as_ref(), next_sibling) {
						error!("Failed to insert comment: {:?}", error);
						continue;
					}
					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_comment.into()))
					}
					1
				}

				lignin::Node::HtmlElement { element, dom_binding } => {
					let &lignin::Element { name, creation_options, .. } = element;
					let span = trace_span!("Creating HTML element", name, ?creation_options, ?dom_binding);
					let _enter = span.enter();

					let dom_element = match match creation_options.is() {
						// This isn't entirely modern, but is well-supported.
						Some(is) => document.create_element_with_str(name, is),
						None => document.create_element(name),
					} {
						Ok(element) => element,
						Err(error) => {
							error!("Failed to create HTML element: {:?}", error);
							continue;
						}
					}
					.dyn_into::<web_sys::HtmlElement>()
					.unwrap_throw();

					if let Err(error) = parent_element.insert_before(dom_element.as_ref(), next_sibling) {
						error!("Failed to insert HTML element: {:?}", error);
						continue;
					}

					self.update_element(
						document,
						&lignin::Element {
							name,
							creation_options,
							attributes: &[],
							content: lignin::Node::Multi(&[]),
							event_bindings: &[],
						},
						element,
						&dom_element,
						depth_limit,
					);

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_element.into()))
					}
					1
				}

				lignin::Node::MathMlElement { element, dom_binding } => {
					let &lignin::Element { name, creation_options, .. } = element;
					let span = trace_span!("Creating MathML element", name, ?creation_options, ?dom_binding);
					let _enter = span.enter();

					let dom_element = match match creation_options.is() {
						// This isn't entirely modern, but is well-supported.
						Some(is) => document.create_element_ns_with_str(Some("http://www.w3.org/1998/Math/MathML"), name, is),
						None => document.create_element_ns(Some("http://www.w3.org/1998/Math/MathML"), name),
					} {
						Ok(element) => element,
						Err(error) => {
							error!("Failed to create MathML element: {:?}", error);
							continue;
						}
					}
					.dyn_into::<web_sys::Element>()
					.unwrap_throw();

					if let Err(error) = parent_element.insert_before(dom_element.as_ref(), next_sibling) {
						error!("Failed to insert MathML element: {:?}", error);
						continue;
					}

					self.update_element(
						document,
						&lignin::Element {
							name,
							creation_options,
							attributes: &[],
							content: lignin::Node::Multi(&[]),
							event_bindings: &[],
						},
						element,
						&dom_element,
						depth_limit,
					);

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_element.into()))
					}
					1
				}

				lignin::Node::SvgElement { element, dom_binding } => {
					let &lignin::Element { name, creation_options, .. } = element;
					let span = trace_span!("Creating SVG element", name, ?creation_options, ?dom_binding);
					let _enter = span.enter();

					let dom_element = match match creation_options.is() {
						// This isn't entirely modern, but is well-supported.
						Some(is) => document.create_element_ns_with_str(Some("http://www.w3.org/2000/svg"), name, is),
						None => document.create_element_ns(Some("http://www.w3.org/2000/svg"), name),
					} {
						Ok(element) => element,
						Err(error) => {
							error!("Failed to create SVG element: {:?}", error);
							continue;
						}
					}
					.dyn_into::<web_sys::SvgElement>()
					.unwrap_throw();

					if let Err(error) = parent_element.insert_before(dom_element.as_ref(), next_sibling) {
						error!("Failed to insert SVG element: {:?}", error);
						continue;
					}

					self.update_element(
						document,
						&lignin::Element {
							name,
							creation_options,
							attributes: &[],
							content: lignin::Node::Multi(&[]),
							event_bindings: &[],
						},
						element,
						&dom_element,
						depth_limit,
					);

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_element.into()))
					}
					1
				}

				lignin::Node::Memoized { state_key, content } => {
					let span = trace_span!("Creating memoized", state_key);
					let _enter = span.enter();
					self.diff_splice_node_list(document, &[], slice::from_ref(content), parent_element, dom_slice, i, depth_limit - 1);
					0
				}
				lignin::Node::Multi(nodes) => {
					let span = trace_span!("Creating multi", "nodes.len()" = nodes.len());
					let _enter = span.enter();
					// May skip `depth_limit` check one level down.
					if !nodes.is_empty() {
						self.diff_splice_node_list(document, &[], nodes, parent_element, dom_slice, i, depth_limit - 1);
					}
					0
				}
				lignin::Node::Keyed(reorderable_fragments) => {
					let span = trace_span!("Creating keyed", "reorderable_fragments.len()" = reorderable_fragments.len());
					let _enter = span.enter();

					debug_assert_eq!(
						reorderable_fragments.len(),
						reorderable_fragments.iter().map(|rf| rf.dom_key).collect::<HashSet<_>>().len(),
						"Duplicate `ReorderableFragment::key` encountered"
					);

					for reorderable_fragment in reorderable_fragments {
						let span = trace_span!("Creating keyed fragment", dom_key = reorderable_fragment.dom_key);
						let _enter = span.enter();
						self.diff_splice_node_list(document, &[], slice::from_ref(&reorderable_fragment.content), parent_element, dom_slice, i, depth_limit - 1)
					}
					0
				}

				lignin::Node::Text { text, dom_binding } => {
					let span = trace_span!("Creating text node", text, ?dom_binding);
					let _enter = span.enter();
					let dom_text = document.create_text_node(text);
					if let Err(error) = parent_element.insert_before(dom_text.as_ref(), next_sibling) {
						error!("Failed to insert text: {:?}", error);
						continue;
					}
					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_text.into()))
					}
					1
				}

				lignin::Node::RemnantSite(_) => {
					let span = trace_span!("Creating remnant site");
					let _enter = span.enter();
					todo!("Create `RemnantSite`")
				}
			};
		}
	}

	/// Decrement event listener handles for (further) missing VDOM nodes, and warn if there were any.
	///
	/// This function is intentionally not quite idiomatic(ally generic) to make sure no copies are generated.
	#[instrument(skip(vdom_a))]
	fn discard_further_missing(&mut self, mut handler_decrement_count: usize, vdom_a: slice::Iter<lignin::Node<'_, ThreadBound>>, depth_limit: usize) {
		handler_decrement_count += vdom_a
			.map(|removed_node| {
				let span = trace_span!("Decrementing listener handles for handlers for further skipped node.");
				let _enter = span.enter();
				self.decrement_handlers(removed_node, depth_limit)
			})
			.sum::<usize>();
		if handler_decrement_count != 0 {
			warn!("{} removed event bindings were unaccounted for.", handler_decrement_count)
		}
	}

	/// Efficiently removes DOM bindings for a to-be-deleted DOM tree without removing them from the DOM.
	#[allow(clippy::never_loop)] // For `'unbound_node: loop`.
	#[allow(clippy::too_many_lines)]
	#[instrument(skip(node))]
	fn unbind_node(&mut self, document: &web_sys::Document, node: &lignin::Node<ThreadBound>, dom_slice: &web_sys::NodeList, i: &mut u32, depth_limit: usize) {
		if depth_limit == 0 {
			return error!("Depth limit reached");
		}

		macro_rules! unbind_element {
			($element:expr, $dom_binding:expr, $debug_kind:literal, $web_type:ty, $($label:tt)+) => {{
				let node = match dom_slice.get(*i) {
					Some(node) => node,
					None => {
						error!(
							"Expected to unbind {} element beyond end of `web_sys::NodeList`. Ignoring bindings.",
							$debug_kind
						);
						break $($label)+ 1;
					}
				};

				let dom_element = match node.dyn_ref::<$web_type>() {
					Some(dom_element) => dom_element,
					None => {
						error!(
							"Expected to unbind `{}` but found {:?}; Ignoring bindings.",
							type_name::<$web_type>(),
							node
						);
						break $($label)+ 1;
					}
				};

				if dom_element.tag_name() != $element.name {
					error!("Expected to unbind <{}> but found <{}>; Ignoring bindings.", $element.name, dom_element.tag_name());
						break $($label)+ 1;
				}

				if let Some(dom_binding) = $dom_binding {
					dom_binding.call(DomRef::Removing(dom_element.into()))
				}

				self.unbind_node(document, &$element.content, &dom_element.child_nodes(), &mut 0, depth_limit - 1);

				1
			}};
		}

		*i += 'unbound_node: loop {
			break 'unbound_node match *node {
				lignin::Node::Comment { comment, dom_binding } => {
					let span = trace_span!("Unbinding comment", comment, ?dom_binding);
					let _enter = span.enter();

					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to unbind comment beyond end of `web_sys::NodeList`. Skipping.");
							break 'unbound_node 1;
						}
					};

					let dom_comment = match node.dyn_ref::<web_sys::Comment>() {
						Some(comment) => comment,
						None => {
							error!("Expected to unbind `web_sys::Comment` but found {:?}; Skipping.", node);
							break 'unbound_node 1;
						}
					};

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Removing(dom_comment.into()))
					}

					if STATIC_MAX_LEVEL >= Level::ERROR && dom_comment.data() != comment {
						error!("Unexpected unbound comment data: {:?}", dom_comment.data())
					}

					1
				}

				lignin::Node::HtmlElement { element, dom_binding } => {
					let span = trace_span!("Unbinding HTML element", tag = element.name, creation_options = ?element.creation_options, ?dom_binding);
					let _enter = span.enter();
					unbind_element!(element, dom_binding, "HTML", web_sys::HtmlElement, 'unbound_node)
				}
				lignin::Node::MathMlElement { element, dom_binding } => {
					let span = trace_span!("Unbinding MathML element", tag = element.name, creation_options = ?element.creation_options, ?dom_binding);
					let _enter = span.enter();
					unbind_element!(element, dom_binding, "MathML", web_sys::Element, 'unbound_node)
				}
				lignin::Node::SvgElement { element, dom_binding } => {
					let span = trace_span!("Unbinding SVG element", tag = element.name, creation_options = ?element.creation_options, ?dom_binding);
					let _enter = span.enter();
					unbind_element!(element, dom_binding, "SVG", web_sys::SvgElement, 'unbound_node)
				}

				lignin::Node::Memoized { state_key, content } => {
					let span = trace_span!("Unbinding memoized", state_key);
					let _enter = span.enter();
					self.unbind_node(document, content, dom_slice, i, depth_limit - 1);
					0
				}
				lignin::Node::Multi(nodes) => {
					let span = trace_span!("Unbinding multi", "nodes.len()" = nodes.len());
					let _enter = span.enter();
					for node in nodes {
						self.unbind_node(document, node, dom_slice, i, depth_limit - 1)
					}
					0
				}
				lignin::Node::Keyed(reorderable_fragments) => {
					let span = trace_span!("Unbinding keyed", "reorderable_fragments.len()" = reorderable_fragments.len());
					let _enter = span.enter();
					for lignin::ReorderableFragment { dom_key, content } in reorderable_fragments {
						let span = trace_span!("Unbinding keyed fragment", dom_key);
						let _enter = span.enter();
						self.unbind_node(document, content, dom_slice, i, depth_limit - 1)
					}
					0
				}

				lignin::Node::Text { text, dom_binding } => {
					let span = trace_span!("Unbinding text node", text, ?dom_binding);
					let _enter = span.enter();
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to unbind text beyond end of `web_sys::NodeList`. Skipping.");
							break 'unbound_node 1;
						}
					};

					let dom_text = match node.dyn_ref::<web_sys::Text>() {
						Some(text) => text,
						None => {
							error!("Expected to unbind `web_sys::Text` but found {:?}; Skipping.", node);
							break 'unbound_node 1;
						}
					};

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Removing(dom_text.into()))
					}

					if STATIC_MAX_LEVEL >= Level::ERROR && dom_text.data() != text {
						error!("Unexpected unbound text data: {:?}", dom_text.data())
					}

					1
				}

				lignin::Node::RemnantSite(_) => {
					let span = trace_span!("Unbinding remnant site");
					let _enter = span.enter();
					todo!("Unbind `RemnantSite`")
				}
			};
		};
	}

	#[instrument(skip(node))]
	// Decrements the reference count for each event handler by 1 per reference in `node`.
	// Returns the total decrement count.
	fn decrement_handlers(&mut self, node: &lignin::Node<ThreadBound>, depth_limit: usize) -> usize {
		if depth_limit == 0 {
			error!("Depth limit reached");
			return 0;
		}

		match *node {
			lignin::Node::Comment { .. } | lignin::Node::Text { .. } => {
				trace!("Nothing to do for comment or text.");
				0
			}
			lignin::Node::HtmlElement { element, dom_binding: _ } | lignin::Node::MathMlElement { element, dom_binding: _ } | lignin::Node::SvgElement { element, dom_binding: _ } => {
				let span = trace_span!("Decrementing listener handles for element", tag = element.name);
				let _enter = span.enter();
				for lignin::EventBinding { callback, .. } in element.event_bindings {
					match self.handler_handles.weak_decrement(callback) {
						Ok(Some(_)) => (),
						Ok(None) => throw_str("Tried to decrement event binding that doesn't exist"),
						Err(rc_hash_map::CountSaturatedError) => throw_str("Tried to decrement handler reference more often than bound"),
					}
				}

				self.decrement_handlers(&element.content, depth_limit - 1)
			}
			lignin::Node::Memoized { state_key, content } => {
				let span = trace_span!("Decrementing listener handles for memoized", state_key);
				let _enter = span.enter();
				self.decrement_handlers(content, depth_limit - 1)
			}
			lignin::Node::Multi(nodes) => {
				let span = trace_span!("Decrementing listener handles for multi", "nodes.len()" = nodes.len());
				let _enter = span.enter();
				nodes.iter().map(|node| self.decrement_handlers(node, depth_limit - 1)).sum()
			}
			lignin::Node::Keyed(reorderable_fragments) => {
				let span = trace_span!("Decrementing listener handles for keyed", "reorderable_fragments.len()" = reorderable_fragments.len());
				let _enter = span.enter();
				reorderable_fragments
					.iter()
					.map(|lignin::ReorderableFragment { dom_key, content }| {
						let span = trace_span!("Decrementing listener handles for keyed fragment", dom_key);
						let _enter = span.enter();
						self.decrement_handlers(content, depth_limit - 1)
					})
					.sum()
			}
			lignin::Node::RemnantSite(_) => {
				let span = trace_span!("Decrementing listener handles for remnant site");
				let _enter = span.enter();
				todo!("Decrement `RemnantSite` event handlers")
			}
		}
	}

	#[allow(clippy::items_after_statements)]
	#[allow(clippy::similar_names)]
	#[instrument(skip(c_1, c_2))]
	fn update_element(
		&mut self,
		document: &web_sys::Document,
		&lignin::Element {
			name: n_1,
			creation_options: co_1,
			attributes: mut a_1,
			content: ref c_1,
			event_bindings: mut eb_1,
		}: &lignin::Element<ThreadBound>,
		&lignin::Element {
			name: n_2,
			creation_options: co_2,
			attributes: mut a_2,
			content: ref c_2,
			event_bindings: mut eb_2,
		}: &lignin::Element<ThreadBound>,
		element: &web_sys::Element,
		depth_limit: usize,
	) {
		debug_assert_eq!(n_1, n_2);
		debug_assert_eq!(co_1, co_2);

		#[instrument]
		fn remove_attribute(attributes: &web_sys::NamedNodeMap, &lignin::Attribute { name, value }: &lignin::Attribute) {
			match attributes.remove_named_item(name) {
				Err(error) => warn!("Could not remove attribute with name {:?}, value {:?}: {:?}", name, value, error),
				Ok(removed) => {
					if STATIC_MAX_LEVEL >= Level::WARN && removed.value() != value {
						warn!("Unexpected value of removed attribute {:?}: Expected {:?} but found {:?}", name, value, removed.value());
					}
				}
			}
		}

		#[instrument]
		fn add_attribute(document: &web_sys::Document, attributes: &web_sys::NamedNodeMap, &lignin::Attribute { name, value }: &lignin::Attribute) {
			let attribute = match document.create_attribute(name) {
				Ok(attribute) => attribute,
				Err(error) => return error!("Could not create attribute {:?}: {:?}", name, error),
			};
			if !value.is_empty() {
				attribute.set_value(value)
			}
			match attributes.set_named_item(&attribute) {
				Ok(None) => (),
				Err(error) => error!("Could not add attribute {:?}={:?}: {:?}", name, value, error),
				Ok(Some(replaced)) => error!("Attribute collision. Added attribute {:?}={:?} was {:?} before", name, value, replaced),
			}
		}

		while !a_1.is_empty() && a_1.first() == a_2.first() {
			a_1 = &a_1[1..];
			a_2 = &a_2[1..];
		}
		while !a_1.is_empty() && a_1.last() == a_2.last() {
			a_1 = &a_1[..a_1.len() - 1];
			a_2 = &a_2[..a_2.len() - 1];
		}
		if !a_1.is_empty() || !a_2.is_empty() {
			let attributes = element.attributes();
			for removed in a_1 {
				remove_attribute(&attributes, removed)
			}

			for added in a_2 {
				add_attribute(document, &attributes, added)
			}
		}

		if STATIC_MAX_LEVEL >= Level::ERROR {
			for (i_a, eb_a) in eb_2.iter().enumerate() {
				for (i_b, eb_b) in eb_2.iter().enumerate() {
					if i_a != i_b && eb_a == eb_b {
						// See <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget/addEventListener#multiple_identical_event_listeners>.
						// Disambiguating duplicates would be expensive, so they aren't supported.
						throw_str(&format!("Duplicate event binding: {:#?}", eb_2))
					}
				}
			}
		}

		while !eb_1.is_empty() && eb_1.first() == eb_2.first() {
			eb_1 = &eb_1[1..];
			eb_2 = &eb_2[1..];
		}
		while !eb_1.is_empty() && eb_1.last() == eb_2.last() {
			eb_1 = &eb_1[..eb_1.len() - 1];
			eb_2 = &eb_2[..eb_2.len() - 1];
		}

		if eb_1.is_empty() {
			for &added in eb_2 {
				self.add_event_listener(element, added)
			}
		} else if eb_2.is_empty() {
			for removed in eb_1 {
				self.remove_event_listener(element, removed)
			}
		} else {
			let event_diff = self.event_binding_diff_set.temp();
			let event_diff: &mut HashSet<EventBinding<ThreadBound>> = unsafe {
				//SAFETY: This field is not otherwise accessed in this block.
				&mut *(event_diff as *mut _)
			};
			for persisting in eb_2 {
				event_diff.insert(*persisting);
			}
			for prior in eb_1 {
				if !event_diff.remove(prior) {
					self.remove_event_listener(element, prior);
				}
			}
			for added in event_diff.drain() {
				self.add_event_listener(element, added);
			}
		}

		self.diff_splice_node_list(document, slice::from_ref(c_1), slice::from_ref(c_2), element, &element.child_nodes(), &mut 0, depth_limit - 1);
	}

	#[instrument]
	fn add_event_listener(&mut self, element: &web_sys::Element, lignin::EventBinding { name, callback, options }: lignin::EventBinding<ThreadBound>) {
		if let Err(error) = element.add_event_listener_with_callback_and_add_event_listener_options(
			name,
			Self::get_or_create_listener(&self.common_handler, &mut self.handler_handles, callback),
			Self::get_cached_add_event_listener_options(&mut self.event_listener_options_cache, options),
		) {
			error!("Failed to add event listener {:?}: {:?}", name, error)
		}
	}

	#[instrument]
	fn remove_event_listener(&mut self, element: &web_sys::Element, removed: &lignin::EventBinding<ThreadBound>) {
		let &lignin::EventBinding { name, ref callback, options } = removed;
		let callback = self.handler_handles.weak_decrement(callback).unwrap_throw().unwrap_throw();
		if let Err(error) = element.remove_event_listener_with_callback_and_bool(name, callback, options.capture()) {
			error!("Failed to remove event listener {:?} ({:?}): {:?}", name, options, error)
		}
	}
}

#[allow(clippy::type_complexity)]
#[must_use]
fn loosen_binding<T>(previous: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>, next: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>, parameter: &T) -> impl '_ + Drop {
	#![allow(clippy::items_after_statements)]

	return if previous == next {
		BindingTransition { next: None, parameter }
	} else {
		if let Some(previous) = previous {
			let span = trace_span!("Unbinding", dom_binding = ?previous);
			let _enter = span.enter();
			previous.call(DomRef::Removing(parameter));
		}
		BindingTransition { next, parameter }
	};

	struct BindingTransition<'a, T>
	where
		fn(DomRef<&'_ T>): CallbackSignature,
	{
		next: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>,
		parameter: &'a T,
	}
	impl<'a, T> Drop for BindingTransition<'a, T>
	where
		fn(DomRef<&'_ T>): CallbackSignature,
	{
		fn drop(&mut self) {
			if let Some(next) = self.next {
				let span = trace_span!("Binding", dom_binding = ?next);
				let _enter = span.enter();
				next.call(DomRef::Added(self.parameter));
			}
		}
	}
}
