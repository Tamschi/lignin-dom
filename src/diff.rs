use core::slice;

use lignin::{callback_registry::CallbackSignature, CallbackRef, DomRef, ThreadBound};
use log::{trace, warn};
use wasm_bindgen::{JsCast, JsValue};

pub fn update_child_nodes(
	document: &web_sys::Document,
	vdom_a: &[lignin::Node<'_, ThreadBound>],
	vdom_b: &[lignin::Node<'_, ThreadBound>],
	dom: &web_sys::Node,
	depth_limit: usize,
) -> Result<(), Error> {
	diff_splice_node_list(
		document,
		vdom_a,
		vdom_b,
		&dom.child_nodes(),
		&mut 0,
		depth_limit,
	)
}

//TODO: This is currently pretty panic-happy on unexpected DOM structure.
#[allow(clippy::too_many_lines)]
fn diff_splice_node_list(
	document: &web_sys::Document,
	vdom_a: &[lignin::Node<'_, ThreadBound>],
	vdom_b: &[lignin::Node<'_, ThreadBound>],
	dom_slice: &web_sys::NodeList,
	i: &mut u32,
	depth_limit: usize,
) -> Result<(), Error> {
	if depth_limit == 0 {
		return Err(Error(ErrorKind::DepthLimitReached));
	}

	let mut vdom_a = vdom_a.iter().copied();
	let mut vdom_b = vdom_b.iter().copied();
	while !vdom_a.len() > 0 && !vdom_b.len() > 0 {
		match (vdom_a.next().unwrap(), vdom_b.next().unwrap()) {
			(
				lignin::Node::Comment {
					comment: c_1,
					dom_binding: db_1,
				},
				lignin::Node::Comment {
					comment: c_2,
					dom_binding: db_2,
				},
			) => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let comment = node.dyn_ref::<web_sys::Comment>().ok_or_else(|| {
					Error(ErrorKind::ExpectedComment {
						found: node.clone(),
					})
				})?;
				let _guard = loosen_binding(db_1, db_2, comment.into());
				if c_1 != c_2 {
					comment.set_data(c_2)
				}
			}

			(
				lignin::Node::HtmlElement {
					element: e_1,
					dom_binding: db_1,
				},
				lignin::Node::HtmlElement {
					element: e_2,
					dom_binding: db_2,
				},
			) if e_1.name == e_2.name => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let element = node.dyn_ref::<web_sys::HtmlElement>().ok_or_else(|| {
					Error(ErrorKind::ExpectedHtmlElement {
						found: node.clone(),
					})
				})?;
				let _guard = loosen_binding(db_1, db_2, element.into());

				update_html_element(document, e_1, e_2, element)?
			}

			(
				lignin::Node::SvgElement {
					element: e_1,
					dom_binding: db_1,
				},
				lignin::Node::SvgElement {
					element: e_2,
					dom_binding: db_2,
				},
			) if e_1.name == e_2.name => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let element = node.dyn_ref::<web_sys::SvgElement>().ok_or_else(|| {
					Error(ErrorKind::ExpectedSvgElement {
						found: node.clone(),
					})
				})?;
				let _guard = loosen_binding(db_1, db_2, element.into());

				todo!("`SvgElement` diff")
			}

			(
				lignin::Node::Memoized {
					state_key: sk_1,
					content: c_1,
				},
				lignin::Node::Memoized {
					state_key: sk_2,
					content: c_2,
				},
			) => {
				if sk_1 != sk_2 {
					diff_splice_node_list(
						document,
						slice::from_ref(c_1),
						slice::from_ref(c_2),
						dom_slice,
						i,
						depth_limit - 1,
					)?
				}
			}

			(lignin::Node::Multi(n_1), lignin::Node::Multi(n_2)) => {
				diff_splice_node_list(document, n_1, n_2, dom_slice, i, depth_limit - 1)?
			}

			(lignin::Node::Keyed(_), lignin::Node::Keyed(_)) => {
				todo!()
			}

			(
				lignin::Node::Text {
					text: t_1,
					dom_binding: db_1,
				},
				lignin::Node::Text {
					text: t_2,
					dom_binding: db_2,
				},
			) => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let text = node.dyn_ref::<web_sys::Text>().ok_or_else(|| {
					Error(ErrorKind::ExpectedText {
						found: node.clone(),
					})
				})?;
				let _guard = loosen_binding(db_1, db_2, text.into());
				if t_1 != t_2 {
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
					if let (
						&lignin::Node::HtmlElement { element: e_1, .. },
						&lignin::Node::HtmlElement { element: e_2, .. },
					)
					| (
						&lignin::Node::SvgElement { element: e_1, .. },
						&lignin::Node::SvgElement { element: e_2, .. },
					) = (n_1, n_2)
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

				diff_splice_node_list(
					document,
					slice::from_ref(n_1),
					&[],
					dom_slice,
					i,
					depth_limit,
				)?;
				diff_splice_node_list(
					document,
					&[],
					slice::from_ref(n_2),
					dom_slice,
					i,
					depth_limit,
				)?;
			}
		}
		*i += 1;
	}

	for removed_node in vdom_a {
		match removed_node {
			lignin::Node::Comment {
				comment,
				dom_binding,
			} => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let comment = node.dyn_ref::<web_sys::Comment>().ok_or_else(|| {
					Error(ErrorKind::ExpectedComment {
						found: node.clone(),
					})
				})?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(comment.into()))
				}

				comment.remove()
			}

			lignin::Node::HtmlElement {
				element,
				dom_binding,
			} => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let html_element = node.dyn_ref::<web_sys::HtmlElement>().ok_or_else(|| {
					Error(ErrorKind::ExpectedHtmlElement {
						found: node.clone(),
					})
				})?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(html_element.into()))
				}

				diff_splice_node_list(
					document,
					slice::from_ref(&element.content),
					&[],
					&html_element.child_nodes(),
					&mut 0,
					depth_limit - 1,
				)?;

				html_element.remove()
			}

			lignin::Node::SvgElement {
				element,
				dom_binding,
			} => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let svg_element = node.dyn_ref::<web_sys::SvgElement>().ok_or_else(|| {
					Error(ErrorKind::ExpectedSvgElement {
						found: node.clone(),
					})
				})?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(svg_element.into()))
				}

				diff_splice_node_list(
					document,
					slice::from_ref(&element.content),
					&[],
					&svg_element.child_nodes(),
					&mut 0,
					depth_limit - 1,
				)?;

				svg_element.remove()
			}

			lignin::Node::Memoized { state_key, content } => diff_splice_node_list(
				document,
				slice::from_ref(content),
				&[],
				dom_slice,
				i,
				depth_limit - 1,
			)?,

			lignin::Node::Multi(nodes) => {
				diff_splice_node_list(document, nodes, &[], dom_slice, i, depth_limit - 1)?
			}

			lignin::Node::Keyed(pairs) => {
				for pair in pairs {
					diff_splice_node_list(
						document,
						slice::from_ref(&pair.content),
						&[],
						dom_slice,
						i,
						depth_limit - 1,
					)?
				}
			}

			lignin::Node::Text { text, dom_binding } => {
				let node = dom_slice
					.get(*i)
					.ok_or_else(|| Error(ErrorKind::NotEnoughDomNodes(dom_slice.clone())))?;
				let text = node.dyn_ref::<web_sys::Text>().ok_or_else(|| {
					Error(ErrorKind::ExpectedComment {
						found: node.clone(),
					})
				})?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(text.into()))
				}

				text.remove()
			}

			lignin::Node::RemnantSite(_) => {
				todo!("Remove `RemnantSite`")
			}
		}
	}

	// let next_child = dom.child_nodes().item(*i); // None if i == dom.child_nodes().length().
	for new_node in vdom_b {
		todo!()
	}

	Ok(())
}

#[allow(clippy::type_complexity)]
#[must_use]
fn loosen_binding<T>(
	previous: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>,
	next: Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>,
	parameter: &T,
) -> impl '_ + Drop {
	#![allow(clippy::items_after_statements)]

	return if previous == next {
		BindingTransition {
			next: None,
			parameter,
		}
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

fn update_html_element(
	document: &web_sys::Document,
	&lignin::Element {
		name: _n_1,
		attributes: mut a_1,
		content: ref c_1,
		event_bindings: mut eb_1,
	}: &lignin::Element<ThreadBound>,
	&lignin::Element {
		name: _n_2,
		attributes: mut a_2,
		content: ref c_2,
		event_bindings: mut eb_2,
	}: &lignin::Element<ThreadBound>,
	element: &web_sys::HtmlElement,
) -> Result<(), Error> {
	debug_assert_eq!(_n_1, _n_2);

	let attributes = element.attributes();
	while !a_1.is_empty() && !a_2.is_empty() {
		todo!()
	}

	for &lignin::Attribute { name, value } in a_1 {
		let attribute = attributes.remove_named_item(name).map_err(|error| {
			Error(ErrorKind::CouldNotRemoveAttribute {
				element: element.clone(),
				name: name.to_owned(),
				error,
			})
		})?;
		if attribute.value() != value {
			return Err(Error(ErrorKind::UnexpectedRemovedAttributeValue {
				element: element.clone(),
				expected: value.to_owned(),
				found: attribute,
			}));
		}
	}

	for &lignin::Attribute { name, value } in a_2 {
		let attribute = document.create_attribute(name).map_err(move |error| {
			Error(ErrorKind::CouldNotCreateAttribute {
				name: name.to_owned(),
				error,
			})
		})?;
		if value != "" {
			attribute.set_value(value)
		}
		if let Some(replaced) = attributes
			.set_named_item(&attribute)
			.map_err(move |error| {
				Error(ErrorKind::CouldNotAddAttribute {
					element: element.clone(),
					attribute,
					error,
				})
			})? {
			return Err(Error(ErrorKind::AttributeCollision {
				element: element.clone(),
				replaced,
			}));
		}
	}

	todo!()
}

pub struct Error(ErrorKind);
enum ErrorKind {
	DepthLimitReached,
	NotEnoughDomNodes(web_sys::NodeList),
	ExpectedComment {
		found: web_sys::Node,
	},
	ExpectedHtmlElement {
		found: web_sys::Node,
	},
	ExpectedSvgElement {
		found: web_sys::Node,
	},
	ExpectedText {
		found: web_sys::Node,
	},
	CouldNotRemoveAttribute {
		element: web_sys::HtmlElement,
		name: String,
		error: JsValue,
	},
	UnexpectedRemovedAttributeValue {
		element: web_sys::HtmlElement,
		expected: String,
		found: web_sys::Attr,
	},
	CouldNotCreateAttribute {
		name: String,
		error: JsValue,
	},
	CouldNotAddAttribute {
		element: web_sys::HtmlElement,
		attribute: web_sys::Attr,
		error: JsValue,
	},
	AttributeCollision {
		element: web_sys::HtmlElement,
		replaced: web_sys::Attr,
	},
}
