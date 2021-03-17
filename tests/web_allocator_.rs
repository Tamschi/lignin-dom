use lignin_dom::load::{self};

pub struct Allocator;
impl load::Allocator<'static> for Allocator {
	fn allocate<T>(&self, instance: T) -> &'static T {
		Box::leak(Box::new(instance))
	}

	fn allocate_slice<T>(&self, iter: &mut dyn ExactSizeIterator<Item = T>) -> &'static [T] {
		iter.collect::<Vec<_>>().leak()
	}
}
