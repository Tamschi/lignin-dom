use lignin::{bumpalo::Bump, Attribute, Element as lElement, Node};
use wasm_bindgen::JsCast;
use web_sys::{Attr, Element, NamedNodeMap, Node as wNode, NodeList, Text};

pub fn load_child_nodes<'a>(child_nodes: &NodeList, bump: &'a Bump) -> &'a [Node<'a>] {
	bump.alloc_slice_fill_with(child_nodes.length() as usize, |i| {
		let child = child_nodes.item(i as u32).unwrap();
		if let Some(element) = child.dyn_ref::<Element>() {
			Node::Element(bump.alloc_with(|| load_element(element, bump)))
		} else if let Some(text) = child.dyn_ref::<Text>() {
			Node::Text(bump.alloc_str(&text.data()))
		} else {
			todo!("unrecognised child node: {:?}", child)
		}
	})
}

pub fn load_element<'a>(element: &Element, bump: &'a Bump) -> lElement<'a> {
	let node: &wNode = element.as_ref();
	lElement {
		name: bump.alloc_str(&element.tag_name()),
		attributes: load_attributes(&element.attributes(), bump),
		content: bump.alloc_with(|| load_child_nodes(&node.child_nodes(), bump)),
		event_bindings: bump.alloc_with(|| []),
	}
}

pub fn load_attributes<'a>(attributes: &NamedNodeMap, bump: &'a Bump) -> &'a [Attribute<'a>] {
	bump.alloc_slice_fill_with(attributes.length() as usize, |i| {
		load_attribute(&attributes.item(i as u32).unwrap(), bump)
	})
}

pub fn load_attribute<'a>(attribute: &Attr, bump: &'a Bump) -> Attribute<'a> {
	Attribute {
		name: bump.alloc_str(&attribute.local_name()),
		value: bump.alloc_str(&attribute.value()),
	}
}
