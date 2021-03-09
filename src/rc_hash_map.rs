use core::{
	borrow::Borrow,
	hash::{BuildHasher, Hash},
};
use hashbrown::{
	hash_map::{DefaultHashBuilder, DrainFilter, Entry},
	HashMap,
};
use num_traits::{CheckedAdd, CheckedSub, One, Zero};

pub struct RcHashMap<K, C, V, S = DefaultHashBuilder>(HashMap<K, (C, V), S>)
where
	K: Hash + Eq,
	C: CheckedAdd + CheckedSub + One + Zero,
	S: BuildHasher;
impl<K, C, V, S> Default for RcHashMap<K, C, V, S>
where
	K: Hash + Eq,
	C: CheckedAdd + CheckedSub + One + Zero,
	S: Default + BuildHasher,
{
	fn default() -> Self {
		Self::new()
	}
}
impl<K, C, V, S> RcHashMap<K, C, V, S>
where
	K: Hash + Eq,
	C: CheckedAdd + CheckedSub + One + Zero,
	S: BuildHasher,
{
	#[must_use]
	pub fn new() -> Self
	where
		S: Default,
	{
		Self(HashMap::with_hasher(S::default()))
	}

	pub fn increment_or_insert_with<F: FnOnce() -> V>(&mut self, k: K, v: F) -> Result<&mut V, CountSaturatedError> {
		match self.0.entry(k) {
			Entry::Occupied(occupied) => {
				let (c, v) = occupied.into_mut();
				*c = c.checked_add(&C::one()).ok_or(CountSaturatedError)?;
				Ok(v)
			}
			Entry::Vacant(vacant) => {
				let (_, v) = vacant.insert((C::one(), v()));
				Ok(v)
			}
		}
	}

	pub fn weak_decrement<Q: ?Sized>(&mut self, k: &Q) -> Result<Option<&mut V>, CountSaturatedError>
	where
		K: Borrow<Q>,
		Q: Eq + Hash,
	{
		match self.0.get_mut(k) {
			Some((c, v)) => {
				*c = c.checked_sub(&C::one()).ok_or(CountSaturatedError)?;
				Ok(Some(v))
			}
			None => Ok(None),
		}
	}

	pub fn drain_weak(&mut self) -> DrainWeak<'_, K, C, V> {
		DrainWeak(self.0.drain_filter(DrainWeak::weak_filter))
	}
}

pub struct DrainWeak<'a, K, C, V>(DrainFilter<'a, K, (C, V), fn(&K, &mut (C, V)) -> bool>);
impl<'a, K, C, V> DrainWeak<'a, K, C, V>
where
	C: Zero,
{
	fn weak_filter(_: &K, (c, _): &mut (C, V)) -> bool {
		c.is_zero()
	}
}
impl<'a, K, C, V> Iterator for DrainWeak<'a, K, C, V> {
	type Item = (K, V);

	fn next(&mut self) -> Option<Self::Item> {
		self.0.next().map(|(k, (_, v))| (k, v))
	}

	fn size_hint(&self) -> (usize, Option<usize>) {
		self.0.size_hint()
	}
}

struct CountSaturatedError;
