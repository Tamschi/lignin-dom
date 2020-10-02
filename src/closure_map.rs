use {
    core::{any::Any, mem::transmute},
    holyhashmap::{Entry, HolyHashMap},
    log::trace,
    std::sync::Mutex,
    wasm_bindgen::closure::Closure,
};

thread_local! {
    #[allow(clippy::type_complexity)]
    static CLOSURE_MAP: Mutex<HolyHashMap<(*const dyn Any, *const dyn Fn(&dyn Any)), Closure<dyn Fn()>>> = Default::default();
}

//TODO: Reference-counting to support closures used more than once.
pub(crate) fn publish(
    context: &'static dyn Any,
    handler: &'static dyn Fn(&dyn Any),
) -> &'static Closure<dyn Fn()> {
    let key = (
        context as *const dyn Any,
        handler as *const dyn Fn(&dyn Any),
    );
    let js_closure = Closure::wrap(Box::new(move || handler(context)) as Box<dyn Fn()>);
    CLOSURE_MAP.with(move |closure_map| {
        let mut closure_map = closure_map.lock().unwrap();
        let entry = closure_map.entry(key);
        let entry = match entry {
            Entry::Vacant(vacant) => vacant,
            Entry::Occupied(_) => todo!("Handle closures that are referenced more than once."),
        };
        let js_closure = entry.insert(js_closure);
        trace!("Created Closure.");
        unsafe {
            //SAFETY: References are only invalidated through `unpublish`, which mustn't be called unless no more references are held.
            extend_reference(js_closure)
        }
    })
}

/// # Safety
///
/// A (target, handler) combination may only be unpublished once for each reference that was acquired from `publish` **and then discarded**.
pub(crate) unsafe fn unpublish(target: &dyn Any, handler: &dyn Fn(&dyn Any)) {
    let key = (
        //SAFETY: Extended references don't exit this function.
        extend_reference(target) as *const dyn Any,
        extend_fn_reference(handler) as *const dyn Fn(&dyn Any),
    );
    CLOSURE_MAP.with(move |closure_map| {
        let mut closure_map = closure_map.lock().unwrap();
        closure_map
            .remove(&key)
            .expect("Unpublished key not found.");
    });
    trace!("Destroyed Closure.");
}

unsafe fn extend_fn_reference<'a, 'b>(
    r#ref: &'a (dyn Fn(&dyn Any) + 'a),
) -> &'b (dyn Fn(&dyn Any) + 'b) {
    transmute(r#ref)
}

unsafe fn extend_reference<'a, 'b, T: ?Sized>(r#ref: &'a T) -> &'b T {
    transmute(r#ref)
}
