use lignin::{CallbackRef, CallbackRegistration, DomRef, Node, ReorderableFragment, ThreadBound};
use lignin_dom::{diff::DomDiffer, load::Allocator as _};
use log::Level;
use std::cell::RefCell;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
use web_sys::{window, HtmlBodyElement};

wasm_bindgen_test_configure!(run_in_browser);

mod web_allocator_;
use web_allocator_::Allocator;

#[wasm_bindgen_test]
fn comment() {
	test_create(
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
	test_create(
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
	test_create(
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
	test_create(
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
	test_create(
		|dom_binding| Node::Memoized {
			state_key: 0,
			content: Allocator.allocate(Node::Text { text: "Hello memoized!", dom_binding }),
		},
		2,
		1,
	);
}

static mut LOG_INITIALIZED: bool = false;

fn test_create<T>(vdom: impl FnOnce(Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>) -> Node<'static, ThreadBound>, depth_limit: usize, binding_count: isize) {
	unsafe {
		if !LOG_INITIALIZED {
			console_log::init_with_level(Level::Trace).unwrap();
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

	differ.update_child_nodes(&[vdom], &[], depth_limit);
	assert_eq!(*ref_count.borrow(), 0);

	drop(callback);
}