use crate::rc_hash_map::{self, RcHashMap};
use core::{any::type_name, slice};
use js_sys::Function;
use lignin::{callback_registry::CallbackSignature, CallbackRef, DomRef, ThreadBound};
use log::{
	debug, error, info, log_enabled, trace, warn,
	Level::{Error, Warn},
};
use std::convert::TryInto;
use wasm_bindgen::{closure::Closure, throw_str, JsCast, JsValue, UnwrapThrowExt};

#[allow(clippy::type_complexity)]
pub struct DomDiffer {
	handler_handles: RcHashMap<CallbackRef<ThreadBound, fn(lignin::web::Event)>, u16, Function>,
	common_handler: Closure<dyn Fn(JsValue, web_sys::Event)>,
	element: web_sys::Element,
	event_listener_options_cache: [Option<web_sys::AddEventListenerOptions>; 8],
}
impl DomDiffer {
	#[must_use]
	pub fn new_for_element_child_nodes(element: web_sys::Element) -> Self {
		Self {
			handler_handles: RcHashMap::new(),
			common_handler: Closure::wrap(Box::new(move |callback_ref: JsValue, event: web_sys::Event| {
				unsafe { CallbackRef::<ThreadBound, fn(lignin::web::Event)>::from_js(&callback_ref) }
					.expect_throw(if cfg!(debug_assertions) {
						//TODO: There's definitely a more elegant way to do this.
						Box::leak(Box::new(format!("lignin-dom bug: Invalid `CallbackRef` {:?}", callback_ref))).as_str()
					} else {
						"lignin-dom bug: Invalid `CallbackRef`. Compile with debug assertions to see the value."
					})
					.call(event.into());
			})),
			element,
			event_listener_options_cache: [None, None, None, None, None, None, None, None],
		}
	}

	fn create_listener_and_get_cached_add_event_listener_options(
		&mut self,
		callback_ref: CallbackRef<ThreadBound, fn(lignin::web::Event)>,
		options: &lignin::EventBindingOptions,
	) -> (&Function, &web_sys::AddEventListenerOptions) {
		let common_handler = &self.common_handler;
		let callback = self
			.handler_handles
			.increment_or_insert_with(callback_ref, |callback_ref| common_handler.as_ref().unchecked_ref::<Function>().bind1(&JsValue::UNDEFINED, &callback_ref.into_js()))
			.expect_throw("Too many (more than 65k) active references to the same `CallbackRef`");

		let options = {
			let entry = self
				.event_listener_options_cache
				.get_mut(options.capture() as usize + options.once() as usize * 2 + options.passive() as usize * 4)
				.unwrap_throw();

			if entry.is_none() {
				let mut web_options = web_sys::AddEventListenerOptions::new();
				web_options.capture(options.capture()).once(options.once()).passive(options.passive());
				*entry = Some(web_options)
			}

			entry.as_ref().unwrap_throw()
		};

		(callback, options)
	}

	pub fn update_child_nodes(&mut self, vdom_a: &[lignin::Node<'_, ThreadBound>], vdom_b: &[lignin::Node<'_, ThreadBound>], depth_limit: usize) {
		let element = self.element.clone();
		let child_nodes = self.element.child_nodes();
		let owner_document = self.element.owner_document().expect_throw("lignin-dom: No owner document found for root element.");
		self.diff_splice_node_list(
			&owner_document,
			vdom_a,
			vdom_b,
			&element,
			&child_nodes,
			&mut 0,
			child_nodes
				.item(lignin::Node::Multi(vdom_a).dom_len().try_into().unwrap_or_else(|_| {
					error!("Overflowed `vdom_a.dom_len()` as u32. Insertions will happen at the very end.");
					u32::MAX
				}))
				.as_ref(),
			depth_limit,
		);

		let drain = self.handler_handles.drain_weak();
		trace!("Freed {} event listener(s).", drain.count())
	}

	#[allow(clippy::too_many_arguments)]
	#[allow(clippy::too_many_lines)]
	fn diff_splice_node_list(
		&mut self,
		document: &web_sys::Document,
		mut vdom_a: &[lignin::Node<'_, ThreadBound>],
		mut vdom_b: &[lignin::Node<'_, ThreadBound>],
		parent_element: &web_sys::Element,
		dom_slice: &web_sys::NodeList,
		i: &mut u32,
		next_sibling: Option<&web_sys::Node>,
		depth_limit: usize,
	) {
		if depth_limit == 0 {
			return error!("Depth limit reached");
		}

		while !vdom_a.is_empty() && !vdom_b.is_empty() {
			#[allow(clippy::never_loop)]
			'vdom_item: loop {
				break 'vdom_item match (vdom_a[0], vdom_b[0]) {
					(lignin::Node::Comment { comment: c_1, dom_binding: db_1 }, lignin::Node::Comment { comment: c_2, dom_binding: db_2 }) => {
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected comment beyond end of `web_sys::NodeList`. Switching to insertions.");
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, next_sibling, depth_limit);
							}
						};

						let comment = match node.dyn_ref::<web_sys::Comment>() {
							Some(comment) => comment,
							None => {
								error!("Expected to update `web_sys::Comment` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::Comment { comment: c_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, next_sibling, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::Comment { comment: c_2, dom_binding: db_2 }], parent_element, dom_slice, i, next_sibling, depth_limit);
								break 'vdom_item;
							}
						};

						let _guard = loosen_binding(db_1, db_2, comment.into());
						if log_enabled!(Error) && comment.data() != c_1 {
							if c_1 == c_2 {
								error!("Unexpected comment data that won't be updated: Expected {:?} but found {:?}", c_1, comment.data(),);
							} else {
								warn!("Unexpected comment data: Expected {:?} but found {:?}", c_1, comment.data(),);
							}
						}
						if c_1 != c_2 {
							comment.set_data(c_2)
						}
					}

					(lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }, lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 })
						if e_1.name == e_2.name && e_1.creation_options == e_2.creation_options =>
					{
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, next_sibling, depth_limit);
							}
						};

						let html_element = match node.dyn_ref::<web_sys::HtmlElement>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::HtmlElement` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(
									document,
									&[lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }],
									&[],
									parent_element,
									dom_slice,
									i,
									next_sibling,
									depth_limit,
								);
								self.diff_splice_node_list(
									document,
									&[],
									&[lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }],
									parent_element,
									dom_slice,
									i,
									next_sibling,
									depth_limit,
								);
								break 'vdom_item;
							}
						};

						if html_element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the HTML element.", e_1.name, html_element.tag_name());
							self.diff_splice_node_list(
								document,
								&[lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }],
								&[],
								parent_element,
								dom_slice,
								i,
								next_sibling,
								depth_limit,
							);
							self.diff_splice_node_list(
								document,
								&[],
								&[lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }],
								parent_element,
								dom_slice,
								i,
								next_sibling,
								depth_limit,
							);
						}

						let _guard = loosen_binding(db_1, db_2, html_element.into());

						self.update_element(document, e_1, e_2, html_element, ElementMode::HtmlOrCustom)
					}

					(lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }, lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 })
						if e_1.name == e_2.name && e_1.creation_options == e_2.creation_options =>
					{
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, next_sibling, depth_limit);
							}
						};

						let element = match node.dyn_ref::<web_sys::Element>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::Element` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(
									document,
									&[lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }],
									&[],
									parent_element,
									dom_slice,
									i,
									next_sibling,
									depth_limit,
								);
								self.diff_splice_node_list(
									document,
									&[],
									&[lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }],
									parent_element,
									dom_slice,
									i,
									next_sibling,
									depth_limit,
								);
								break 'vdom_item;
							}
						};

						if element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the MathML element.", e_1.name, element.tag_name());
							self.diff_splice_node_list(
								document,
								&[lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }],
								&[],
								parent_element,
								dom_slice,
								i,
								next_sibling,
								depth_limit,
							);
							self.diff_splice_node_list(
								document,
								&[],
								&[lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }],
								parent_element,
								dom_slice,
								i,
								next_sibling,
								depth_limit,
							);
						}

						let _guard = loosen_binding(db_1, db_2, element.into());

						self.update_element(document, e_1, e_2, element, ElementMode::MathMl)
					}

					(lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }, lignin::Node::SvgElement { element: e_2, dom_binding: db_2 })
						if e_1.name == e_2.name && e_1.creation_options == e_2.creation_options =>
					{
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, next_sibling, depth_limit);
							}
						};

						let svg_element = match node.dyn_ref::<web_sys::SvgElement>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::SvgElement` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(
									document,
									&[lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }],
									&[],
									parent_element,
									dom_slice,
									i,
									next_sibling,
									depth_limit,
								);
								self.diff_splice_node_list(
									document,
									&[],
									&[lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }],
									parent_element,
									dom_slice,
									i,
									next_sibling,
									depth_limit,
								);
								break 'vdom_item;
							}
						};

						if svg_element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the SVG element.", e_1.name, svg_element.tag_name());
							self.diff_splice_node_list(
								document,
								&[lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }],
								&[],
								parent_element,
								dom_slice,
								i,
								next_sibling,
								depth_limit,
							);
							self.diff_splice_node_list(
								document,
								&[],
								&[lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }],
								parent_element,
								dom_slice,
								i,
								next_sibling,
								depth_limit,
							);
						}

						let _guard = loosen_binding(db_1, db_2, svg_element.into());

						self.update_element(document, e_1, e_2, svg_element, ElementMode::Svg)
					}

					(lignin::Node::Memoized { state_key: sk_1, content: c_1 }, lignin::Node::Memoized { state_key: sk_2, content: c_2 }) => {
						if sk_1 != sk_2 {
							self.diff_splice_node_list(document, slice::from_ref(c_1), slice::from_ref(c_2), parent_element, dom_slice, i, next_sibling, depth_limit - 1)
						}
					}

					(lignin::Node::Multi(n_1), lignin::Node::Multi(n_2)) => self.diff_splice_node_list(document, n_1, n_2, parent_element, dom_slice, i, next_sibling, depth_limit - 1),

					(lignin::Node::Keyed(_), lignin::Node::Keyed(_)) => {
						todo!()
					}

					(lignin::Node::Text { text: t_1, dom_binding: db_1 }, lignin::Node::Text { text: t_2, dom_binding: db_2 }) => {
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected comment beyond end of `web_sys::NodeList`. Switching to insertions.");
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, parent_element, dom_slice, i, next_sibling, depth_limit);
							}
						};

						let text = match node.dyn_ref::<web_sys::Text>() {
							Some(comment) => comment,
							None => {
								error!("Expected to update `web_sys::Text` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::Text { text: t_1, dom_binding: db_1 }], &[], parent_element, dom_slice, i, next_sibling, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::Text { text: t_2, dom_binding: db_2 }], parent_element, dom_slice, i, next_sibling, depth_limit);
								break 'vdom_item;
							}
						};

						let _guard = loosen_binding(db_1, db_2, text.into());
						if text.data() != t_1 {
							error!("Unexpected text data: Expected {:?} but found {:?}. Overwriting.", t_1, text.data(),);
						} else if t_1 != t_2 {
							text.set_data(t_2)
						}
					}

					(lignin::Node::RemnantSite(_), lignin::Node::RemnantSite(_)) => {
						todo!("`RemnantSite` diff")
					}

					// Mismatching nodes: Destroy and rebuild.
					(ref n_1, ref n_2) => {
						trace!("Replace mismatching");

						if cfg!(debug_assertions) && log::STATIC_MAX_LEVEL >= log::Level::Warn {
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

						self.diff_splice_node_list(document, slice::from_ref(n_1), &[], parent_element, dom_slice, i, next_sibling, depth_limit);
						self.diff_splice_node_list(document, &[], slice::from_ref(n_2), parent_element, dom_slice, i, next_sibling, depth_limit);
					}
				};
			}

			vdom_a = &vdom_a[1..];
			vdom_b = &vdom_b[1..];
		}

		let mut vdom_a = vdom_a.iter();
		for removed_node in vdom_a.by_ref() {
			self.decrement_handlers(removed_node, depth_limit);

			macro_rules! remove_element {
				($element:expr, $dom_binding:expr, $debug_kind:literal, $web_type:ty) => {{
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!(
								"Expected to remove {} element beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.",
								$debug_kind
							);
							for removed_node in vdom_a {
								trace!("Decrementing handlers for further skipped node.");
								self.decrement_handlers(removed_node, depth_limit)
							}
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
					trace!("Removing comment.");
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to remove comment beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.");
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

					if log_enabled!(Error) && dom_comment.data() != comment {
						error!("Unexpected removed comment data: {:?}", dom_comment.data())
					}

					dom_comment.remove();
				}

				lignin::Node::HtmlElement { element, dom_binding } => {
					trace!("Removing HTML element <{:?}>:", element.name);
					remove_element!(element, dom_binding, "HTML", web_sys::HtmlElement)
				}

				lignin::Node::MathMlElement { element, dom_binding } => {
					trace!("Removing MathML element <{:?}>:", element.name);
					remove_element!(element, dom_binding, "MathML", web_sys::Element)
				}

				lignin::Node::SvgElement { element, dom_binding } => {
					trace!("Removing SVG element <{:?}>:", element.name);
					remove_element!(element, dom_binding, "SVG", web_sys::SvgElement)
				}

				lignin::Node::Memoized { state_key, content } => {
					trace!("Removing memoized {:?}:", state_key);
					self.diff_splice_node_list(document, slice::from_ref(content), &[], parent_element, dom_slice, i, next_sibling, depth_limit - 1)
				}

				lignin::Node::Multi(nodes) => {
					trace!("Removing multi - start");
					self.diff_splice_node_list(document, nodes, &[], parent_element, dom_slice, i, next_sibling, depth_limit - 1);
					trace!("Removing multi - end");
				}

				lignin::Node::Keyed(reorderable_fragments) => {
					trace!("Removing keyed - start");
					for reorderable_fragment in reorderable_fragments {
						trace!("Removing keyed - item {:?}:", reorderable_fragment.dom_key);
						self.diff_splice_node_list(document, slice::from_ref(&reorderable_fragment.content), &[], parent_element, dom_slice, i, next_sibling, depth_limit - 1)
					}
					trace!("Removing keyed - end");
				}

				lignin::Node::Text { text, dom_binding } => {
					trace!("Removing text node.");
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to remove text beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.");
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

					if log_enabled!(Error) && dom_text.data() != text {
						error!("Unexpected removed text data: {:?}", dom_text.data())
					}

					dom_text.remove();
				}

				lignin::Node::RemnantSite(_) => {
					todo!("Remove `RemnantSite`")
				}
			}
		}

		for new_node in vdom_b {
			match *new_node {
				lignin::Node::Comment { comment, dom_binding } => {
					trace!("Creating comment.");
					let dom_comment = document.create_comment(comment);
					if let Err(error) = parent_element.insert_before(dom_comment.as_ref(), next_sibling) {
						error!("Failed to insert comment: {:?}", error);
						continue;
					}
					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_comment.into()))
					}
				}
				lignin::Node::HtmlElement { element, dom_binding } => {
					let &lignin::Element { name, creation_options, .. } = element;
					trace!("Creating HTML element <{:?}>:", name);

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
						ElementMode::HtmlOrCustom,
					);

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_element.into()))
					}
				}
				lignin::Node::MathMlElement { element, dom_binding } => {
					let &lignin::Element { name, creation_options, .. } = element;
					trace!("Creating MathML element <{:?}>:", name);

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
						ElementMode::MathMl,
					);

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_element.into()))
					}
				}
				lignin::Node::SvgElement { element, dom_binding } => {
					let &lignin::Element { name, creation_options, .. } = element;
					trace!("Creating SVG element <{:?}>:", name);

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
						ElementMode::Svg,
					);

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_element.into()))
					}
				}
				lignin::Node::Memoized { state_key, content } => {
					trace!("Creating memoized {:?}:", state_key);
					self.diff_splice_node_list(document, &[], slice::from_ref(content), parent_element, dom_slice, i, next_sibling, depth_limit - 1);
				}
				lignin::Node::Multi(nodes) => {
					trace!("Creating multi - start");
					self.diff_splice_node_list(document, &[], nodes, parent_element, dom_slice, i, next_sibling, depth_limit - 1);
					trace!("Creating multi - end");
				}
				lignin::Node::Keyed(reorderable_fragments) => {
					trace!("Creating keyed - start");
					for reorderable_fragment in reorderable_fragments {
						trace!("Creating keyed - item {:?}:", reorderable_fragment.dom_key);
						self.diff_splice_node_list(document, &[], slice::from_ref(&reorderable_fragment.content), parent_element, dom_slice, i, next_sibling, depth_limit - 1)
					}
					trace!("Creating keyed - end");
				}
				lignin::Node::Text { text, dom_binding } => {
					trace!("Creating text node.");
					let dom_text = document.create_text_node(text);
					if let Err(error) = parent_element.insert_before(dom_text.as_ref(), next_sibling) {
						error!("Failed to insert text: {:?}", error);
						continue;
					}
					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Added(&dom_text.into()))
					}
				}
				lignin::Node::RemnantSite(_) => {
					todo!("Create `RemnantSite`")
				}
			}
			*i += 1;
		}
	}

	fn unbind_node(&mut self, document: &web_sys::Document, node: &lignin::Node<ThreadBound>, dom_slice: &web_sys::NodeList, i: &mut u32, depth_limit: usize) {
		if depth_limit == 0 {
			return error!("Depth limit reached");
		}

		match *node {
			lignin::Node::Comment { comment, dom_binding } => {
				trace!("Unbinding comment.");
				let node = match dom_slice.get(*i) {
					Some(node) => node,
					None => {
						error!("Expected to unbind comment beyond end of `web_sys::NodeList`. Skipping.");
						return;
					}
				};

				let dom_comment = match node.dyn_ref::<web_sys::Comment>() {
					Some(comment) => comment,
					None => {
						error!("Expected to unbind `web_sys::Comment` but found {:?}; Skipping.", node);
						return;
					}
				};

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(dom_comment.into()))
				}

				if log_enabled!(Error) && dom_comment.data() != comment {
					error!("Unexpected unbound comment data: {:?}", dom_comment.data())
				}
			}

			lignin::Node::HtmlElement { element, dom_binding } => {
				trace!("Unbinding HTML element <{:?}>:", element.name);
				todo!()
			}
			lignin::Node::MathMlElement { element, dom_binding } => {
				trace!("Unbinding MathML element <{:?}>:", element.name);
				todo!()
			}
			lignin::Node::SvgElement { element, dom_binding } => {
				trace!("Unbinding SVG element <{:?}>:", element.name);
				todo!()
			}

			lignin::Node::Memoized { state_key: _, content } => self.unbind_node(document, content, dom_slice, i, depth_limit - 1),
			lignin::Node::Multi(nodes) => {
				trace!("Unbinding multi - start");
				for node in nodes {
					self.unbind_node(document, node, dom_slice, i, depth_limit - 1)
				}
				trace!("Unbinding multi - end");
			}
			lignin::Node::Keyed(reorderable_fragments) => {
				trace!("Unbinding keyed - start");
				for lignin::ReorderableFragment { dom_key: _, content } in reorderable_fragments {
					self.unbind_node(document, content, dom_slice, i, depth_limit - 1)
				}
				trace!("Unbinding keyed - end");
			}

			lignin::Node::Text { text, dom_binding } => {
				trace!("Unbinding text.");
				let node = match dom_slice.get(*i) {
					Some(node) => node,
					None => {
						error!("Expected to unbind text beyond end of `web_sys::NodeList`. Skipping.");
						return;
					}
				};

				let dom_text = match node.dyn_ref::<web_sys::Text>() {
					Some(text) => text,
					None => {
						error!("Expected to unbind `web_sys::Text` but found {:?}; Skipping.", node);
						return;
					}
				};

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(dom_text.into()))
				}

				if log_enabled!(Error) && dom_text.data() != text {
					error!("Unexpected unbound text data: {:?}", dom_text.data())
				}
			}

			lignin::Node::RemnantSite(_) => {
				todo!("Unbind `RemnantSite`")
			}
		}
	}

	fn decrement_handlers(&mut self, node: &lignin::Node<ThreadBound>, depth_limit: usize) {
		if depth_limit == 0 {
			return error!("Depth limit reached");
		}

		match *node {
			lignin::Node::Comment { .. } | lignin::Node::Text { .. } => trace!("Nothing to do for comment or text."),
			lignin::Node::HtmlElement { element, dom_binding: _ } | lignin::Node::MathMlElement { element, dom_binding: _ } | lignin::Node::SvgElement { element, dom_binding: _ } => {
				trace!("Decrementing element <{:?}>:", element.name);
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
				trace!("Decrementing memoized {:?}:", state_key);
				self.decrement_handlers(content, depth_limit - 1)
			}
			lignin::Node::Multi(nodes) => {
				trace!("Decrementing multi - start");
				for node in nodes {
					self.decrement_handlers(node, depth_limit - 1)
				}
				trace!("Decrementing multi - end");
			}
			lignin::Node::Keyed(reorderable_fragments) => {
				trace!("Decrementing keyed - start");
				for lignin::ReorderableFragment { dom_key: _, content } in reorderable_fragments {
					self.decrement_handlers(content, depth_limit - 1)
				}
				trace!("Decrementing keyed - end");
			}
			lignin::Node::RemnantSite(_) => {
				todo!("Decrement `RemnantSite` event handlers")
			}
		}
	}

	#[allow(clippy::items_after_statements)]
	#[allow(clippy::similar_names)]
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
		mode: ElementMode,
	) {
		trace!("Updating element ({:?}) - start", mode);

		debug_assert_eq!(n_1, n_2);
		debug_assert_eq!(co_1, co_2);

		fn remove_attribute(element: &web_sys::Element, attributes: &web_sys::NamedNodeMap, &lignin::Attribute { name, value }: &lignin::Attribute, mode: ElementMode) {
			trace!("Removing attribute {:?}={:?}.", name, value);
			match attributes.remove_named_item(name) {
				Err(error) => warn!("Could not remove attribute with name {:?}, value {:?}: {:?}", name, value, error),
				Ok(removed) => {
					if log_enabled!(Warn) && removed.value() != value {
						warn!("Unexpected value of removed attribute {:?}: Expected {:?} but found {:?}", name, value, removed.value());
					}
				}
			}
		}

		fn add_attribute(document: &web_sys::Document, element: &web_sys::Element, attributes: &web_sys::NamedNodeMap, &lignin::Attribute { name, value }: &lignin::Attribute, mode: ElementMode) {
			trace!("Adding attribute {:?}={:?}.", name, value);
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

		let attributes = element.attributes();
		while !a_1.is_empty() && !a_2.is_empty() {
			todo!()
		}

		for removed in a_1 {
			remove_attribute(element, &attributes, removed, mode)
		}

		for added in a_2 {
			add_attribute(document, element, &attributes, added, mode)
		}

		for lignin::EventBinding { name, callback, options } in eb_1.iter().filter(|eb| !eb_2.contains(eb)) {
			trace!("Removing event listener {:?} ({:?}).", name, options);
			let callback = self.handler_handles.weak_decrement(callback).unwrap_throw().unwrap_throw();
			if let Err(error) = element.remove_event_listener_with_callback_and_bool(name, callback, options.capture()) {
				error!("Failed to remove event listener {:?} ({:?}): {:?}", name, options, error)
			}
		}

		for lignin::EventBinding { name, callback, ref options } in eb_2.iter().copied().filter(|eb| !eb_1.contains(eb)) {
			trace!("Adding event listener {:?} ({:?}).", name, options);
			let (callback, options) = self.create_listener_and_get_cached_add_event_listener_options(callback, options);
			if let Err(error) = element.add_event_listener_with_callback_and_add_event_listener_options(name, callback, options) {
				error!("Failed to remove event listener {:?}: {:?}", name, error)
			}
		}

		if !c_1.dom_empty() || !c_2.dom_empty() {
			todo!()
		}

		trace!("Updating element ({:?}) - end", mode);
	}
}

/// Controls how certain attributes are namespaced.
#[derive(Debug, Clone, Copy, PartialEq)]
enum ElementMode {
	HtmlOrCustom,
	MathMl,
	Svg,
}

#[allow(clippy::type_complexity)]
#[must_use]
fn loosen_binding<'a, T>(previous: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>, next: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>, parameter: &'a T) -> impl 'a + Drop {
	#![allow(clippy::items_after_statements)]

	return if previous == next {
		BindingTransition { next: None, parameter }
	} else {
		if let Some(previous) = previous {
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
				next.call(DomRef::Added(self.parameter));
			}
		}
	}
}
