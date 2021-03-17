use std::cell::RefCell;

use lignin::{CallbackRef, CallbackRegistration, DomRef, Node, ThreadBound};
use lignin_dom::diff::DomDiffer;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
use web_sys::{window, HtmlBodyElement};

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn comment() {
	test_create(|dom_binding| Node::Comment {
		comment: "Hello lignin-dom!",
		dom_binding,
	});
}

#[wasm_bindgen_test]
fn text() {
	test_create(|dom_binding| Node::Text {
		text: "Hello lignin-dom!",
		dom_binding,
	});
}

fn test_create<T>(
	vdom: impl FnOnce(Option<CallbackRef<ThreadBound, fn(DomRef<&'_ T>)>>) -> Node<'static, ThreadBound>,
) {
	let body = window()
		.unwrap()
		.document()
		.unwrap()
		.body()
		.unwrap()
		.dyn_into::<HtmlBodyElement>()
		.unwrap();

	let mut differ = DomDiffer::new_for_element_child_nodes(body.into());

	let got_ref = Box::pin(RefCell::new(0));

	let callback =
		CallbackRegistration::<_, fn(DomRef<&'_ T>)>::new(got_ref.as_ref(), |got_ref, _| {
			*unsafe { got_ref.as_ref() }.unwrap().borrow_mut() += 1
		});

	let dom_binding = Some(callback.to_ref_thread_bound());
	differ.update_child_nodes(&[], &[vdom(dom_binding)], 1);

	drop(callback);

	assert_eq!(*got_ref.borrow(), 1);
}
