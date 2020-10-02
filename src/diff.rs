use {
    crate::closure_map,
    core::{any::Any, mem::transmute},
    lignin::{Element, Node},
    log::trace,
    wasm_bindgen::JsCast,
    web_sys::{Comment as wComment, Element as wElement, Node as wNode, Text as wText},
};

#[cfg(feature = "remnants")]
use lignin::remnants::RemnantSite;

/// # Safety
///
/// Any DOM structure created by this function must be either:
///
/// - destroyed before 'b ends or
/// - updated such that `vdom_b` in this call is passed as `vdom_a` in that call.
pub unsafe fn update_child_nodes<'a, 'b, 'd>(
    vdom_a: &'a [Node<'a>],
    vdom_b: &'b [Node<'b>],
    dom: &'d wNode,
) {
    update_child_nodes_at(vdom_a, vdom_b, dom, &mut 0)
}

/// # Safety
///
/// Any DOM structure created by this function must be either:
///
/// - destroyed before 'b ends or
/// - updated such that `vdom_b` in this call is passed as `vdom_a` in that call.
//TODO: This is currently pretty panic-happy on unexpected DOM structure.
unsafe fn update_child_nodes_at<'a, 'b, 'd>(
    mut vdom_a: &'a [Node<'a>],
    mut vdom_b: &'b [Node<'b>],
    dom: &'d wNode,
    i: &mut u32,
) {
    let dom_children = dom.child_nodes();
    while !vdom_a.is_empty() && !vdom_b.is_empty() {
        use Node::*;
        match (
            &vdom_a[0],
            &vdom_b[0],
            &dom_children.item(*i).expect("TODO"),
        ) {
            (Comment(a), Comment(b), d) => {
                if a != b {
                    d.dyn_ref::<wComment>().expect("TODO").set_data(b);
                }
            }
            (Text(a), Text(b), d) => {
                if a != b {
                    d.dyn_ref::<wText>().expect("TODO").set_data(b);
                }
            }
            (Element(a), Element(b), d) if a.name == b.name => {
                update_element(a, b, d.dyn_ref::<wElement>().expect("TODO"));
            }
            abi => todo!("Unhandled child node diff: {:?}", abi),
        };
        vdom_a = &vdom_a[1..];
        vdom_b = &vdom_b[1..];
        *i += 1;
    }
    for removed_node in vdom_a {
        match *removed_node {
            Node::Comment(_) | Node::Element(_) | Node::Text(_) => {
                dom.remove_child(&dom.child_nodes().item(*i).expect("TODO"))
                    .unwrap();
                //SAFETY: Once the child is gone from the DOM, there should be no way to still call this event.
                //TODO?: Call this only after creating new nodes so that reference counting works better?
                unpublish_closures(removed_node);
            }
            Node::Ref(&referenced) => update_child_nodes_at(&[referenced], &[], dom, i),
            Node::Multi(multi) => update_child_nodes_at(multi, &[], dom, i),
            #[cfg(feature = "remnants")]
            Node::RemnantSite(&RemnantSite { content, .. }) => {
                update_child_nodes_at(&[*content], &[], dom, i)
            }
            _ => todo!("Removing this kind of Node is unimplemented"),
        }
    }

    let document = dom.owner_document().expect("TODO: No owner document.");
    let next_child = dom.child_nodes().item(*i); // None if i == dom.child_nodes().length().
    for new_node in vdom_b {
        use Node::*;
        let new_node: Option<wNode> = match new_node {
            Comment(c) => Some(document.create_comment(c).into()),
            Text(t) => Some(document.create_text_node(t).into()),
            Element(e) => {
                let element = document.create_element(e.name).expect("TODO");
                for a in e.attributes {
                    element.set_attribute(a.name, a.value).unwrap();
                }
                update_child_nodes(&[], e.content, element.as_ref());
                for event_binding in e.event_bindings {
                    trace!("Binding event {:?}", event_binding.name);
                    element
                        .add_event_listener_with_callback(
                            event_binding.name,
                            closure_map::publish(
                                //SAFETY: See safety section in this function's documentation.
                                extend_reference(event_binding.context),
                                extend_fn_reference(event_binding.handler),
                            )
                            .as_ref()
                            .unchecked_ref(),
                        )
                        .expect("TODO")
                }
                Some(element.into())
            }
            Multi(m) => {
                update_child_nodes_at(&[], m, dom, i);
                None
            }
            new_node => todo!("Unhandled new node: {:?}", new_node),
        };
        if let Some(new_node) = new_node {
            dom.insert_before(&new_node, next_child.as_ref()).unwrap();
            *i += 1;
        }
    }
}

/// # Safety
///
/// Any DOM structure created by this function must be either:
///
/// - destroyed before 'b ends or
/// - updated such that `vdom_b` in this call is passed as `vdom_a` in that call.
//TODO: This is currently pretty panic-happy on unexpected DOM structure.
pub unsafe fn update_element<'a, 'b, 'd>(
    vdom_a: &'a Element<'a>,
    vdom_b: &'b Element<'b>,
    dom: &'d wElement,
) {
    update_child_nodes(vdom_a.content, vdom_b.content, dom);
    todo!("Update attributes and event bindings.");
}

unsafe fn unpublish_closures(node: &Node<'_>) {
    match *node {
        Node::Comment(_) | Node::Text(_) => (),
        Node::Ref(reference) => unpublish_closures(reference),
        #[cfg(feature = "remnants")]
        Node::RemnantSite(RemnantSite { content, .. }) => unpublish_closures(content),
        Node::Multi(multi) => {
            for node in multi {
                unpublish_closures(node)
            }
        }
        Node::Element(&Element {
            content,
            event_bindings,
            ..
        }) => {
            for child in content {
                unpublish_closures(child)
            }
            for event_binding in event_bindings {
                trace!("unpublishing closure for event {:?}", event_binding.name);
                closure_map::unpublish(event_binding.context, event_binding.handler)
            }
        }
        _ => todo!(),
    }
}

unsafe fn extend_fn_reference<'a, 'b>(
    r#ref: &'a (dyn Fn(&dyn Any) + 'a),
) -> &'b (dyn Fn(&dyn Any) + 'b) {
    transmute(r#ref)
}

unsafe fn extend_reference<'a, 'b, T: ?Sized>(r#ref: &'a T) -> &'b T {
    transmute(r#ref)
}
