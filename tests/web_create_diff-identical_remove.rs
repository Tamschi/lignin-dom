use lignin::{web::Event, CallbackRef, CallbackRegistration, DomRef, Element, ElementCreationOptions, EventBinding, EventBindingOptions, Node, ReorderableFragment, ThreadBound};
use lignin_dom::{diff::DomDiffer, load::Allocator as _};
use std::cell::RefCell;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
use web_sys::{window, HtmlBodyElement};

wasm_bindgen_test_configure!(run_in_browser);

mod web_allocator_;
use web_allocator_::Allocator;

#[wasm_bindgen_test]
fn comment() {
	test_create_diff_identical_remove(
		|dom_binding| Node::Comment {
			comment: "Hello lignin-dom comment!",
			dom_binding,
		},
		1,
		1,
	);
}

#[wasm_bindgen_test]
fn text() {
	test_create_diff_identical_remove(
		|dom_binding| Node::Text {
			text: "Hello lignin-dom text!",
			dom_binding,
		},
		1,
		1,
	);
}

#[wasm_bindgen_test]
fn multi() {
	test_create_diff_identical_remove(
		|dom_binding| {
			Node::Multi(Allocator.allocate([
				Node::Text { text: "Hello lignin-dom", dom_binding },
				Node::Text { text: " multiple ", dom_binding },
				Node::Text { text: "nodes!", dom_binding },
			]))
		},
		2,
		3,
	);
}

#[wasm_bindgen_test]
fn keyed() {
	test_create_diff_identical_remove(
		|dom_binding| {
			Node::Keyed(Allocator.allocate([
				ReorderableFragment {
					dom_key: 0,
					content: Node::Text { text: "Hello lignin-dom", dom_binding },
				},
				ReorderableFragment {
					dom_key: 0, // Intentionally the same as above.
					content: Node::Text { text: " keyed ", dom_binding },
				},
				ReorderableFragment {
					dom_key: 1,
					content: Node::Text { text: " nodes.", dom_binding },
				},
			]))
		},
		2,
		3,
	);
}

#[wasm_bindgen_test]
fn memoized() {
	test_create_diff_identical_remove(
		|dom_binding| Node::Memoized {
			state_key: 0,
			content: Allocator.allocate(Node::Text { text: "Hello memoized!", dom_binding }),
		},
		2,
		1,
	);
}

#[wasm_bindgen_test]
fn minimal_div() {
	test_create_diff_identical_remove(
		|dom_binding| Node::HtmlElement {
			element: Allocator.allocate(Element {
				name: "DIV",
				creation_options: ElementCreationOptions::new(),
				attributes: &[],
				content: Node::Multi(&[]),
				event_bindings: &[],
			}),
			dom_binding,
		},
		2,
		1,
	);
}

#[wasm_bindgen_test]
fn clickable_div() {
	let clicked = CallbackRegistration::<_, fn(Event)>::new(Box::pin(()).as_ref(), |_, _| ());
	test_create_diff_identical_remove(
		|dom_binding| Node::HtmlElement {
			element: Allocator.allocate(Element {
				name: "DIV",
				creation_options: ElementCreationOptions::new(),
				attributes: &[],
				content: Node::Multi(&[]),
				event_bindings: Allocator.allocate([EventBinding {
					name: "click",
					callback: clicked.to_ref_thread_bound(),
					options: EventBindingOptions::new(),
				}]),
			}),
			dom_binding,
		},
		2,
		1,
	);
}

#[wasm_bindgen_test]
fn minimal_math() {
	test_create_diff_identical_remove(
		|dom_binding| Node::MathMlElement {
			element: Allocator.allocate(Element {
				name: "math",
				creation_options: ElementCreationOptions::new(),
				attributes: &[],
				content: Node::Multi(&[]),
				event_bindings: &[],
			}),
			dom_binding,
		},
		2,
		1,
	);
}

#[wasm_bindgen_test]
fn minimal_svg() {
	test_create_diff_identical_remove(
		|dom_binding| Node::SvgElement {
			element: Allocator.allocate(Element {
				name: "svg",
				creation_options: ElementCreationOptions::new(),
				attributes: &[],
				content: Node::Multi(&[]),
				event_bindings: &[],
			}),
			dom_binding,
		},
		2,
		1,
	);
}

static mut LOG_INITIALIZED: bool = false;

fn test_create_diff_identical_remove<'a, T>(vdom: impl FnOnce(Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>) -> Node<'a, ThreadBound>, depth_limit: usize, binding_count: isize) {
	unsafe {
		if !LOG_INITIALIZED {
			//TODO: Fail on Warnig or Error.
			tracing_wasm::set_as_global_default();
			LOG_INITIALIZED = true;
		}
	}

	let body = window().unwrap().document().unwrap().body().unwrap().dyn_into::<HtmlBodyElement>().unwrap();

	let mut differ = DomDiffer::new_for_element_child_nodes(body.into());

	let ref_count = Box::pin(RefCell::new(0));

	let callback = CallbackRegistration::<_, fn(DomRef<&'_ T>)>::new(ref_count.as_ref(), |got_ref, dom_ref| {
		*unsafe { got_ref.as_ref() }.unwrap().borrow_mut() += match dom_ref {
			DomRef::Added(_) => 1,
			DomRef::Removing(_) => -1,
		}
	});

	let dom_binding = Some(callback.to_ref_thread_bound());
	let vdom = vdom(dom_binding);

	differ.update_child_nodes(&[], &[vdom], depth_limit);
	assert_eq!(*ref_count.borrow(), binding_count);

	differ.update_child_nodes(&[vdom], &[vdom], depth_limit);
	assert_eq!(*ref_count.borrow(), binding_count);

	differ.update_child_nodes(&[vdom], &[], depth_limit);
	assert_eq!(*ref_count.borrow(), 0);

	drop(callback);
}
