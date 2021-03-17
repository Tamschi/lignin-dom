use core::ptr;
use std::{
	borrow::BorrowMut,
	cell::{Cell, RefCell},
};

use lignin::{web::Comment, CallbackRegistration, DomRef, Node, ThreadBound};
use lignin_dom::diff::DomDiffer;
use wasm_bindgen::JsCast;
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
use web_sys::{window, HtmlBodyElement};

wasm_bindgen_test_configure!(run_in_browser);

#[wasm_bindgen_test]
fn comment() {
	let body = window().unwrap().document().unwrap().body().unwrap().dyn_into::<HtmlBodyElement>().unwrap();

	let mut differ = DomDiffer::new_for_element_child_nodes(body.into());

	let got_ref = Box::pin(RefCell::new(0));

	let callback = CallbackRegistration::<_, fn(DomRef<&'_ Comment>)>::new(got_ref.as_ref(), |got_ref, _| *unsafe { got_ref.as_ref() }.unwrap().borrow_mut() += 1);

	differ.update_child_nodes(
		&[],
		&[Node::Comment {
			comment: "Hello lignin-dom!",
			dom_binding: Some(callback.to_ref_thread_bound()),
		}],
		1,
	);

	drop(callback);

	assert_eq!(*got_ref.borrow(), 1);
}
