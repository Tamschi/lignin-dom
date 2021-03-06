#![allow(clippy::module_name_repetitions)]

use lignin::ThreadSafe;
use std::{convert::TryInto, iter};
use wasm_bindgen::JsCast;

pub trait Allocator<'a> {
	fn allocate<T>(&self, instance: T) -> &'a T;
	fn allocate_slice<T>(&self, iter: &dyn ExactSizeIterator<Item = T>) -> &'a [T];
}

struct SliceGenerator<T, NextAt: FnMut(usize) -> T> {
	len: usize,
	counter: usize,
	next_at: NextAt,
}
impl<T, NextAt: FnMut(usize) -> T> SliceGenerator<T, NextAt> {
	pub fn new(len: usize, next_at: NextAt) -> Self {
		Self {
			len,
			counter: 0,
			next_at,
		}
	}
}
impl<T, NextAt: FnMut(usize) -> T> Iterator for SliceGenerator<T, NextAt> {
	type Item = T;

	fn next(&mut self) -> Option<Self::Item> {
		(self.counter < self.len).then(|| {
			let i = self.counter;
			self.counter = i + 1;
			(self.next_at)(i)
		})
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		let remaining = self.len - self.counter;
		(remaining, Some(remaining))
	}
}
impl<T, NextAt: FnMut(usize) -> T> ExactSizeIterator for SliceGenerator<T, NextAt> {
	fn len(&self) -> usize {
		self.size_hint().0
	}
}

pub fn load_child_nodes<'a, A: Allocator<'a>>(
	allocator: &A,
	child_nodes: &web_sys::NodeList,
) -> lignin::Node<'a, ThreadSafe> {
	match child_nodes.length() {
		1 => load_node(allocator, &child_nodes.item(0).unwrap()),
		len => lignin::Node::Multi(
			allocator.allocate_slice(&SliceGenerator::new(len as usize, |i| {
				load_node(allocator, &child_nodes.item(i.try_into().unwrap()).unwrap())
			})),
		),
	}
}

pub fn load_node<'a, A: Allocator<'a>>(
	allocator: &A,
	node: &web_sys::Node,
) -> lignin::Node<'a, ThreadSafe> {
	if let Some(element) = node.dyn_ref::<web_sys::Element>() {
		lignin::Node::Element {
			element: allocator.allocate(load_element(allocator, element)),
			dom_binding: None,
		}
	} else if let Some(text) = node.dyn_ref::<web_sys::Text>() {
		lignin::Node::Text {
			text: allocator.allocate(text.data()),
			dom_binding: (None),
		}
	} else if let Some(comment) = node.dyn_ref::<web_sys::Comment>() {
		lignin::Node::Comment {
			comment: allocator.allocate(comment.data()),
			dom_binding: None,
		}
	} else {
		unreachable!("Impossible node {:?}", node)
	}
}

pub fn load_element<'a, A: Allocator<'a>>(
	allocator: &A,
	element: &web_sys::Element,
) -> lignin::Element<'a, ThreadSafe> {
	let node: &web_sys::Node = element.as_ref();
	lignin::Element {
		name: allocator.allocate(element.tag_name()),
		attributes: load_attributes(allocator, &element.attributes()),
		content: load_child_nodes(allocator, &node.child_nodes()),
		event_bindings: allocator.allocate_slice(&iter::empty()),
	}
}

pub fn load_attributes<'a, A: Allocator<'a>>(
	allocator: &A,
	attributes: &web_sys::NamedNodeMap,
) -> &'a [lignin::Attribute<'a>] {
	allocator.allocate_slice(&SliceGenerator::new(attributes.length() as usize, |i| {
		load_attribute(allocator, &attributes.item(i.try_into().unwrap()).unwrap())
	}))
}

pub fn load_attribute<'a, A: Allocator<'a>>(
	allocator: &A,
	attribute: &web_sys::Attr,
) -> lignin::Attribute<'a> {
	lignin::Attribute {
		name: allocator.allocate(attribute.local_name()),
		value: allocator.allocate(attribute.value()),
	}
}
