use lignin::{CallbackRef, CallbackRegistration, DomRef, Node, ThreadBound};
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
		1,
	);
}

static mut log_init: bool = false;

fn test_create<T>(vdom: impl FnOnce(Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>) -> Node<'static, ThreadBound>, binding_count: usize) {
	unsafe {
		if log_init == false {
			console_log::init_with_level(Level::Trace).unwrap();
			log_init = true;
		}
	}

	let body = window().unwrap().document().unwrap().body().unwrap().dyn_into::<HtmlBodyElement>().unwrap();

	let mut differ = DomDiffer::new_for_element_child_nodes(body.into());

	let got_ref = Box::pin(RefCell::new(0));

	let callback = CallbackRegistration::<_, fn(DomRef<&'_ T>)>::new(got_ref.as_ref(), |got_ref, _| *unsafe { got_ref.as_ref() }.unwrap().borrow_mut() += 1);

	let dom_binding = Some(callback.to_ref_thread_bound());
	differ.update_child_nodes(&[], &[vdom(dom_binding)], 1000);

	drop(callback);

	assert_eq!(*got_ref.borrow(), binding_count);
}
