use hashbrown::HashSet;
use lignin::{EventBinding, ThreadBound};

pub struct TempEventBindingSet(HashSet<EventBinding<'static, ThreadBound>>);
impl TempEventBindingSet {
	pub fn new() -> Self {
		Self(HashSet::new())
	}

	pub fn temp<'a>(&mut self) -> &mut HashSet<EventBinding<'a, ThreadBound>> {
		unsafe {
			//SAFETY: The collection is cleared before each borrow, so no values can leak between them.
			// Note that clearing after each borrow instead would NOT be (particularly) safe without poisoning,
			// as globals in the Wasm instance could be re-referenced after an abort via re-entry from JS.
			self.0.clear();
			&mut *(&mut self.0 as *mut HashSet<EventBinding<'static, ThreadBound>>).cast()
		}
	}

	/// Retrieves the cache set's capacity without clearing it first.
	pub fn capacity(&self) -> usize {
		self.0.capacity()
	}
}
