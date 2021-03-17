use crate::rc_hash_map::{self, RcHashMap};
use core::slice;
use js_sys::Function;
use lignin::{callback_registry::CallbackSignature, CallbackRef, DomRef, Materialize, ThreadBound};
use log::{
	debug, error, info, log_enabled, trace, warn,
	Level::{Error, Warn},
};
use std::{cell::UnsafeCell, marker::PhantomPinned, mem::MaybeUninit, pin::Pin};
use wasm_bindgen::{closure::Closure, throw_str, JsCast, JsValue, UnwrapThrowExt};

#[allow(clippy::type_complexity)]
pub struct DomDiffer {
	handler_handles: RcHashMap<CallbackRef<ThreadBound, fn(lignin::web::Event)>, u16, Function>,
	common_handler: Closure<dyn Fn(JsValue, web_sys::Event)>,
	node_list: web_sys::NodeList,
	_pin: PhantomPinned,
}
impl DomDiffer {
	#[must_use]
	pub fn new_for_element_child_nodes(element: &web_sys::Element) -> Self {
		Self::new_for_node_list(element.child_nodes().clone())
	}
	#[must_use]
	pub fn new_for_node_list(node_list: web_sys::NodeList) -> Self {
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
			node_list,
			_pin: PhantomPinned,
		}
	}

	fn create_listener(&mut self, callback_ref: CallbackRef<ThreadBound, fn(lignin::web::Event)>) -> &Function {
		let common_handler = &self.common_handler;
		self.handler_handles
			.increment_or_insert_with(callback_ref, |callback_ref| {
				common_handler.as_ref().unchecked_ref::<Function>().bind1(&JsValue::UNDEFINED, &callback_ref.into_js())
			})
			.expect_throw("Too many (more than 65k) active references to the same `CallbackRef`")
	}

	pub fn update_child_nodes(
		&mut self,
		document: &web_sys::Document,
		vdom_a: &[lignin::Node<'_, ThreadBound>],
		vdom_b: &[lignin::Node<'_, ThreadBound>],
		dom: &web_sys::Node,
		depth_limit: usize,
	) {
		self.diff_splice_node_list(document, vdom_a, vdom_b, &dom.child_nodes(), &mut 0, depth_limit);
		self.handler_handles.drain_weak();
	}

	#[allow(clippy::too_many_lines)]
	fn diff_splice_node_list(
		&mut self,
		document: &web_sys::Document,
		mut vdom_a: &[lignin::Node<'_, ThreadBound>],
		mut vdom_b: &[lignin::Node<'_, ThreadBound>],
		dom_slice: &web_sys::NodeList,
		i: &mut u32,
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
								return self.diff_splice_node_list(document, &[], vdom_b, dom_slice, i, depth_limit);
							}
						};

						let comment = match node.dyn_ref::<web_sys::Comment>() {
							Some(comment) => comment,
							None => {
								error!("Expected to update `web_sys::Comment` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::Comment { comment: c_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::Comment { comment: c_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
								break 'vdom_item;
							}
						};

						let _guard = self.loosen_binding(db_1, db_2, comment.into());
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

					(lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }, lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }) if e_1.name == e_2.name => {
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, dom_slice, i, depth_limit);
							}
						};

						let html_element = match node.dyn_ref::<web_sys::HtmlElement>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::HtmlElement` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
								break 'vdom_item;
							}
						};

						if html_element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the HTML element.", e_1.name, html_element.tag_name());
							self.diff_splice_node_list(document, &[lignin::Node::HtmlElement { element: e_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
							self.diff_splice_node_list(document, &[], &[lignin::Node::HtmlElement { element: e_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
						}

						let _guard = self.loosen_binding(db_1, db_2, html_element.into());

						self.update_element(document, e_1, e_2, html_element, ElementMode::HtmlOrCustom)
					}

					(lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }, lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }) if e_1.name == e_2.name => {
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, dom_slice, i, depth_limit);
							}
						};

						let element = match node.dyn_ref::<web_sys::Element>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::Element` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
								break 'vdom_item;
							}
						};

						if element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the MathML element.", e_1.name, element.tag_name());
							self.diff_splice_node_list(document, &[lignin::Node::MathMlElement { element: e_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
							self.diff_splice_node_list(document, &[], &[lignin::Node::MathMlElement { element: e_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
						}

						let _guard = self.loosen_binding(db_1, db_2, element.into());

						self.update_element(document, e_1, e_2, element, ElementMode::MathMl)
					}

					(lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }, lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }) if e_1.name == e_2.name => {
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected <{}> beyond end of `web_sys::NodeList`. Switching to insertions.", e_1.name);
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, dom_slice, i, depth_limit);
							}
						};

						let svg_element = match node.dyn_ref::<web_sys::SvgElement>() {
							Some(element) => element,
							None => {
								error!("Expected to update `web_sys::SvgElement` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
								break 'vdom_item;
							}
						};

						if svg_element.tag_name() != e_1.name {
							error!("Expected to update <{}> but found <{}>; Recreating the SVG element.", e_1.name, svg_element.tag_name());
							self.diff_splice_node_list(document, &[lignin::Node::SvgElement { element: e_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
							self.diff_splice_node_list(document, &[], &[lignin::Node::SvgElement { element: e_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
						}

						let _guard = self.loosen_binding(db_1, db_2, svg_element.into());

						self.update_element(document, e_1, e_2, svg_element, ElementMode::Svg)
					}

					(lignin::Node::Memoized { state_key: sk_1, content: c_1 }, lignin::Node::Memoized { state_key: sk_2, content: c_2 }) => {
						if sk_1 != sk_2 {
							self.diff_splice_node_list(document, slice::from_ref(c_1), slice::from_ref(c_2), dom_slice, i, depth_limit - 1)
						}
					}

					(lignin::Node::Multi(n_1), lignin::Node::Multi(n_2)) => self.diff_splice_node_list(document, n_1, n_2, dom_slice, i, depth_limit - 1),

					(lignin::Node::Keyed(_), lignin::Node::Keyed(_)) => {
						todo!()
					}

					(lignin::Node::Text { text: t_1, dom_binding: db_1 }, lignin::Node::Text { text: t_2, dom_binding: db_2 }) => {
						let node = match dom_slice.get(*i) {
							Some(node) => node,
							None => {
								error!("Expected comment beyond end of `web_sys::NodeList`. Switching to insertions.");
								// TODO: Decrement event listener handles by vdom_a. Info about the total count.
								return self.diff_splice_node_list(document, &[], vdom_b, dom_slice, i, depth_limit);
							}
						};

						let text = match node.dyn_ref::<web_sys::Text>() {
							Some(comment) => comment,
							None => {
								error!("Expected to update `web_sys::Text` but found {:?}; Recreating the node.", node);
								self.diff_splice_node_list(document, &[lignin::Node::Text { text: t_1, dom_binding: db_1 }], &[], dom_slice, i, depth_limit);
								self.diff_splice_node_list(document, &[], &[lignin::Node::Text { text: t_2, dom_binding: db_2 }], dom_slice, i, depth_limit);
								break 'vdom_item;
							}
						};

						let _guard = self.loosen_binding(db_1, db_2, text.into());
						if text.data() != t_1 {
							error!("Uself.nexpected text data: Expected {:?} but found {:?}. Overwriting.", t_1, text.data(),);
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

						self.diff_splice_node_list(document, slice::from_ref(n_1), &[], dom_slice, i, depth_limit);
						self.diff_splice_node_list(document, &[], slice::from_ref(n_2), dom_slice, i, depth_limit);
					}
				};
			}

			vdom_a = &vdom_a[1..];
			vdom_b = &vdom_b[1..];
		}

		for removed_node in vdom_a {
			match *removed_node {
				lignin::Node::Comment { comment, dom_binding } => {
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
							error!(
								"Expected to remove `web_sys::Comment` but found {:?}; (Inefficiently) deleting the node anyway but ignoring bindings.",
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

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Removing(dom_comment.into()))
					}

					if log_enabled!(Error) && dom_comment.data() != comment {
						error!("Unexpected removed comment data: {:?}", dom_comment.data())
					}

					dom_comment.remove();
				}

				lignin::Node::HtmlElement { element, dom_binding } => {
					let node = match dom_slice.get(*i) {
						Some(node) => node,
						None => {
							error!("Expected to remove HTML element beyond end of `web_sys::NodeList`. Skipping further deletions here while ignoring bindings.");
							//TODO: Decrement bindings.
							break;
						}
					};

					let html_element = match node.dyn_ref::<web_sys::HtmlElement>() {
						Some(html_element) => html_element,
						None => {
							error!(
								"Expected to remove `web_sys::HtmlElement` but found {:?}; (Inefficiently) deleting the node anyway but ignoring bindings.",
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

					if html_element.tag_name() != element.name {
						error!(
							"Expected to remove <{}> but found <{}>; Removing anyway but ignoring bindings.",
							element.name,
							html_element.tag_name()
						);
						html_element.remove();
						continue;
					}

					if let Some(dom_binding) = dom_binding {
						dom_binding.call(DomRef::Removing(html_element.into()))
					}

					self.unbind_node(document, &element.content, &html_element.child_nodes(), &mut 0, depth_limit - 1);

					html_element.remove()
				}

				lignin::Node::MathMlElement { element, dom_binding } => {
					todo!("Remove MathML element")
				}

				lignin::Node::SvgElement { element, dom_binding } => {
					todo!("Remove SVG element")
				}

				lignin::Node::Memoized { state_key: _, content } => self.diff_splice_node_list(document, slice::from_ref(content), &[], dom_slice, i, depth_limit - 1),

				lignin::Node::Multi(nodes) => self.diff_splice_node_list(document, nodes, &[], dom_slice, i, depth_limit - 1),

				lignin::Node::Keyed(pairs) => {
					for pair in pairs {
						self.diff_splice_node_list(document, slice::from_ref(&pair.content), &[], dom_slice, i, depth_limit - 1)
					}
				}

				lignin::Node::Text { text, dom_binding } => {
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
							error!(
								"Expected to remove `web_sys::Text` but found {:?}; (Inefficiently) deleting the node anyway but ignoring bindings.",
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

		// let next_child = dom.child_nodes().item(*i); // None if i == dom.child_nodes().length().
		for new_node in vdom_b {
			todo!();
			*i += 1;
		}
	}

	#[allow(clippy::type_complexity)]
	#[must_use]
	fn loosen_binding<'a, T>(
		&mut self,
		previous: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>,
		next: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>,
		parameter: &'a T,
	) -> impl 'a + Drop {
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

	fn unbind_node(&mut self, document: &web_sys::Document, node: &lignin::Node<ThreadBound>, dom_slice: &web_sys::NodeList, i: &mut u32, depth_limit: usize) {
		if depth_limit == 0 {
			return error!("Depth limit reached");
		}

		match *node {
			lignin::Node::Comment { comment, dom_binding } => {
				todo!()
			}
			lignin::Node::HtmlElement { element, dom_binding } => {
				todo!()
			}
			lignin::Node::MathMlElement { element, dom_binding } => {
				todo!()
			}
			lignin::Node::SvgElement { element, dom_binding } => {
				todo!()
			}
			lignin::Node::Memoized { state_key: _, content } => self.unbind_node(document, content, dom_slice, i, depth_limit - 1),
			lignin::Node::Multi(nodes) => {
				for node in nodes {
					self.unbind_node(document, node, dom_slice, i, depth_limit - 1)
				}
			}
			lignin::Node::Keyed(reorderable_fragments) => {
				for lignin::ReorderableFragment { dom_key: _, content } in reorderable_fragments {
					self.unbind_node(document, content, dom_slice, i, depth_limit - 1)
				}
			}
			lignin::Node::Text { text, dom_binding } => {
				todo!()
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
			lignin::Node::Comment { .. } | lignin::Node::Text { .. } => (),
			lignin::Node::HtmlElement { element, dom_binding: _ }
			| lignin::Node::MathMlElement { element, dom_binding: _ }
			| lignin::Node::SvgElement { element, dom_binding: _ } => {
				for lignin::EventBinding { callback, .. } in element.event_bindings {
					match self.handler_handles.weak_decrement(callback) {
						Ok(Some(_)) => (),
						Ok(None) => throw_str("Tried to decrement event binding that doesn't exist"),
						Err(rc_hash_map::CountSaturatedError) => throw_str("Tried to decrement handler reference more often than bound"),
					}
				}
			}
			lignin::Node::Memoized { state_key: _, content } => self.decrement_handlers(content, depth_limit - 1),
			lignin::Node::Multi(nodes) => {
				for node in nodes {
					self.decrement_handlers(node, depth_limit - 1)
				}
			}
			lignin::Node::Keyed(reorderable_fragements) => {
				for lignin::ReorderableFragment { dom_key: _, content } in reorderable_fragements {
					self.decrement_handlers(content, depth_limit - 1)
				}
			}
			lignin::Node::RemnantSite(_) => {
				todo!("Decrement `RemnantSite` event handlers")
			}
		}
	}

	#[allow(clippy::items_after_statements)]
	fn update_element(
		&mut self,
		document: &web_sys::Document,
		&lignin::Element {
			name: n_1,
			attributes: mut a_1,
			content: ref c_1,
			event_bindings: mut eb_1,
		}: &lignin::Element<ThreadBound>,
		&lignin::Element {
			name: n_2,
			attributes: mut a_2,
			content: ref c_2,
			event_bindings: mut eb_2,
		}: &lignin::Element<ThreadBound>,
		element: &web_sys::Element,
		mode: ElementMode,
	) {
		debug_assert_eq!(n_1, n_2);

		fn remove_attribute(element: &web_sys::Element, attributes: &web_sys::NamedNodeMap, &lignin::Attribute { name, value }: &lignin::Attribute, mode: ElementMode) {
			match attributes.remove_named_item(name) {
				Err(error) => warn!("Could not remove attribute with name {:?}, value {:?}: {:?}", name, value, error),
				Ok(removed) => {
					if log_enabled!(Warn) && removed.value() != value {
						warn!("Unexpected value of removed attribute {:?}: Expected {:?} but found {:?}", name, value, removed.value());
					}
				}
			}
		}

		fn add_attribute(
			document: &web_sys::Document,
			element: &web_sys::Element,
			attributes: &web_sys::NamedNodeMap,
			&lignin::Attribute { name, value }: &lignin::Attribute,
			mode: ElementMode,
		) {
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

		todo!()
	}
}

/// Controls how certain attributes are namespaced.
#[derive(Clone, Copy, PartialEq)]
enum ElementMode {
	HtmlOrCustom,
	MathMl,
	Svg,
}
