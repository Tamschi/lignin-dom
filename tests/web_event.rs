use lignin::{web::Event, CallbackRegistration, Element, ElementCreationOptions, EventBinding, EventBindingOptions, Materialize, Node};
use lignin_dom::diff::DomDiffer;
use std::cell::RefCell;
use wasm_bindgen::{JsCast, JsValue};
use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};
use web_sys::{window, HtmlBodyElement, HtmlElement};

wasm_bindgen_test_configure!(run_in_browser);

static mut LOG_INITIALIZED: bool = false;

#[wasm_bindgen_test]
fn click() {
	unsafe {
		if !LOG_INITIALIZED {
			//TODO: Fail on Warnig or Error.
			tracing_wasm::set_as_global_default();
			LOG_INITIALIZED = true;
		}
	}

	let body = window().unwrap().document().unwrap().body().unwrap().dyn_into::<HtmlBodyElement>().unwrap();

	let mut differ = DomDiffer::new_for_element_child_nodes(body.into());

	let attributes = [lignin::Attribute { name: "id", value: "test-button" }];

	let click_count = Box::pin(RefCell::new(0));

	let callback = CallbackRegistration::<_, fn(Event)>::new(click_count.as_ref(), |click_count, event| {
		let event = event.materialize();
		let event: &JsValue = event.as_ref();
		event.dyn_ref::<web_sys::Event>().expect("Expected Event but reeived something else.");

		*unsafe { click_count.as_ref() }.unwrap().borrow_mut() += 1;
	});

	let bindings = [EventBinding {
		name: "click",
		callback: callback.to_ref_thread_bound(),
		options: EventBindingOptions::new(),
	}];

	let element = Element {
		name: "BUTTON",
		creation_options: ElementCreationOptions::new(),
		attributes: &attributes,
		content: Node::Multi(&[]),
		event_bindings: &bindings,
	};

	let vdom = Node::HtmlElement { element: &element, dom_binding: None };

	//TODO: Skip the depth limit check differently for empty multi nodes so that 1 works here.
	assert_eq!(*click_count.borrow(), 0);
	differ.update_child_nodes(&[], &[vdom], 2);
	assert_eq!(*click_count.borrow(), 0);

	let button: HtmlElement = window().unwrap().document().unwrap().get_element_by_id("test-button").unwrap().dyn_into().unwrap();
	button.click();

	assert_eq!(*click_count.borrow(), 1);
	differ.update_child_nodes(&[vdom], &[], 2);
	assert_eq!(*click_count.borrow(), 1);
}
