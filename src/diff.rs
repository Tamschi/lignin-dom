use core::slice;

use lignin::{CallbackRef, DomRef, ThreadBound};
use log::{trace, warn};
use wasm_bindgen::JsCast;

pub fn update_child_nodes(
	vdom_a: &[lignin::Node<'_, ThreadBound>],
	vdom_b: &[lignin::Node<'_, ThreadBound>],
	dom: &web_sys::Node,
	depth_limit: usize,
) -> Result<(), Error> {
	diff_splice_node_list(vdom_a, vdom_b, &dom.child_nodes(), &mut 0, depth_limit)
}

//TODO: This is currently pretty panic-happy on unexpected DOM structure.
fn diff_splice_node_list(
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
				let comment = node
					.dyn_ref::<web_sys::Comment>()
					.ok_or_else(|| Error(ErrorKind::ExpectedComment(node.clone())))?;
				let guard = unlock(db_1, db_2, &mut || comment.clone().into());
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
				let element = node
					.dyn_ref::<web_sys::HtmlElement>()
					.ok_or_else(|| Error(ErrorKind::ExpectedElement(node.clone())))?;
				let guard = unlock(db_1, db_2, &mut || element.clone().into());

				todo!("`HtmlElement` diff")
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
				let element = node
					.dyn_ref::<web_sys::SvgElement>()
					.ok_or_else(|| Error(ErrorKind::ExpectedElement(node.clone())))?;
				let guard = unlock(db_1, db_2, &mut || element.clone().into());

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
						slice::from_ref(c_1),
						slice::from_ref(c_2),
						dom_slice,
						i,
						depth_limit - 1,
					)?
				}
			}

			(lignin::Node::Multi(n_1), lignin::Node::Multi(n_2)) => {
				diff_splice_node_list(n_1, n_2, dom_slice, i, depth_limit - 1)?
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
				let text = node
					.dyn_ref::<web_sys::Text>()
					.ok_or_else(|| Error(ErrorKind::ExpectedText(node.clone())))?;
				let guard = unlock(db_1, db_2, &mut || text.clone().into());
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

				diff_splice_node_list(slice::from_ref(n_1), &[], dom_slice, i, depth_limit)?;
				diff_splice_node_list(&[], slice::from_ref(n_2), dom_slice, i, depth_limit)?;
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
				let comment = node
					.dyn_ref::<web_sys::Comment>()
					.ok_or_else(|| Error(ErrorKind::ExpectedComment(node.clone())))?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(comment.clone().into()))
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
				let html_element = node
					.dyn_ref::<web_sys::HtmlElement>()
					.ok_or_else(|| Error(ErrorKind::ExpectedElement(node.clone())))?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(html_element.clone().into()))
				}

				diff_splice_node_list(
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
				let svg_element = node
					.dyn_ref::<web_sys::SvgElement>()
					.ok_or_else(|| Error(ErrorKind::ExpectedElement(node.clone())))?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(svg_element.clone().into()))
				}

				diff_splice_node_list(
					slice::from_ref(&element.content),
					&[],
					&svg_element.child_nodes(),
					&mut 0,
					depth_limit - 1,
				)?;

				svg_element.remove()
			}

			lignin::Node::Memoized { state_key, content } => {
				diff_splice_node_list(slice::from_ref(content), &[], dom_slice, i, depth_limit - 1)?
			}

			lignin::Node::Multi(nodes) => {
				diff_splice_node_list(nodes, &[], dom_slice, i, depth_limit - 1)?
			}

			lignin::Node::Keyed(pairs) => {
				for pair in pairs {
					diff_splice_node_list(
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
				let text = node
					.dyn_ref::<web_sys::Text>()
					.ok_or_else(|| Error(ErrorKind::ExpectedComment(node.clone())))?;

				if let Some(dom_binding) = dom_binding {
					dom_binding.call(DomRef::Removing(text.clone().into()))
				}

				text.remove()
			}

			lignin::Node::RemnantSite(_) => {
				todo!("Remove `RemnantSite`")
			}
		}
	}

	let document = dom.owner_document().expect("TODO: No owner document.");
	let next_child = dom.child_nodes().item(*i); // None if i == dom.child_nodes().length().
	for new_node in vdom_b {
		todo!()
	}

	Ok(())
}

//FIXME: It would generally be better to pass JS objects by reference here.
#[must_use]
fn unlock<'a, T>(
	previous: Option<CallbackRef<ThreadBound, DomRef<T>>>,
	next: Option<CallbackRef<ThreadBound, DomRef<T>>>,
	get_parameter: &'a mut dyn FnMut() -> T,
) -> impl 'a + Drop {
	#![allow(clippy::items_after_statements)]

	return if previous == next {
		BindingTransition {
			next: None,
			get_parameter,
		}
	} else {
		if let Some(previous) = previous {
			previous.call(DomRef::Removing(get_parameter()));
		}
		BindingTransition {
			next,
			get_parameter,
		}
	};

	struct BindingTransition<'a, T> {
		next: Option<CallbackRef<ThreadBound, DomRef<T>>>,
		get_parameter: &'a mut dyn FnMut() -> T,
	}
	impl<'a, T> Drop for BindingTransition<'a, T> {
		fn drop(&mut self) {
			if let Some(next) = self.next {
				next.call(DomRef::Added((self.get_parameter)()));
			}
		}
	}
}

pub struct Error(ErrorKind);
enum ErrorKind {
	DepthLimitReached,
	NotEnoughDomNodes(web_sys::NodeList),
	ExpectedComment(web_sys::Node),
	ExpectedElement(web_sys::Node),
	ExpectedText(web_sys::Node),
}
