use core::fmt;
use core::fmt::{Debug, Display};
use core::hash::Hash;
use core::marker::PhantomData;
use core::ops::{BitAnd, BitOr};

/// Implementation of bitset. See [`Set`]
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
pub struct Cons<B, S>(PhantomData<(B, S)>);

impl<B: Display + Default, S: Display + Default> Display for Cons<B, S> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"{}{}",
			<S as Default>::default(),
			<B as Default>::default()
		)
	}
}

/// Represents a bitset represented as zero.
/// Only if a bitset equals to [`Bit0`], the bitset means zero.
/// See [`Set`] for details.
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
pub struct Bit0;
impl Display for Bit0 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "0")
	}
}

/// Represents a bitset represented as one.
/// See [`Set`] for details.
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
pub struct Bit1;
impl Display for Bit1 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "1")
	}
}

/// The main trait represents a bitset.
pub trait Set: Copy + Clone + Default + Eq + PartialEq + Debug + Hash {
	/// Integer representation of the bitset.
	const N: usize;
}

impl Set for Bit0 {
	const N: usize = 0;
}

impl Set for Bit1 {
	const N: usize = 1;
}

impl<B: Bit, S: Positive> Set for Cons<B, S> {
	const N: usize = <S as Set>::N * 2 + 1;
}

/// A trait implemented if the bitset is positive (not zero).
pub trait Positive: Set {}
impl Positive for Bit1 {}
impl<B: Bit, S: Positive> Positive for Cons<B, S> {}

/// A trait which represents a bit.
pub trait Bit: Set {}
impl Bit for Bit0 {}
impl Bit for Bit1 {}

/// Generate left shift of the bitset.
pub trait ShiftRaising {
	type Output;
}

impl ShiftRaising for Bit0 {
	type Output = Bit0;
}

impl ShiftRaising for Bit1 {
	type Output = Cons<Bit0, Bit1>;
}

impl<B, S> ShiftRaising for Cons<B, S>
where
	Cons<B, S>: Set,
{
	type Output = Cons<Bit0, Cons<B, S>>;
}

/// Generate right shift of the bitset.
pub trait ShiftLowering {
	type Output;
}

impl ShiftLowering for Bit0 {
	type Output = Bit0;
}

impl ShiftLowering for Bit1 {
	type Output = Bit0;
}

impl<B, S> ShiftLowering for Cons<B, S>
where
	Cons<B, S>: Set,
{
	type Output = S;
}

/// Make left shift of the bitset and use give bit as the LSB.
pub trait Push<B> {
	type Output: Set;
}

impl<B: Bit> Push<B> for Bit0 {
	type Output = B;
}

impl<B: Bit> Push<B> for Bit1 {
	type Output = Cons<B, Bit1>;
}

impl<B0: Bit, B: Bit, S: Positive> Push<B0> for Cons<B, S> {
	type Output = Cons<B0, Cons<B, S>>;
}

impl<B: Bit> BitAnd<B> for Bit1 {
	type Output = B;
	fn bitand(self, _: B) -> Self::Output {
		B::default()
	}
}

impl<B: Bit> BitOr<B> for Bit1 {
	type Output = Bit1;
	fn bitor(self, _: B) -> Self::Output {
		Bit1
	}
}

impl<B: Bit> BitAnd<B> for Bit0 {
	type Output = Bit0;
	fn bitand(self, _: B) -> Self::Output {
		Bit0
	}
}

impl<B: Bit> BitOr<B> for Bit0 {
	type Output = B;
	fn bitor(self, _: B) -> Self::Output {
		B::default()
	}
}

impl<
	B1: Bit,
	B2: Bit + BitOr<B1>,
	S1,
	S2: BitOr<S1>
> BitOr<Cons<B1, S1>> for Cons<B2, S2> {
	type Output = Cons<B2::Output, S2::Output>;

	fn bitor(self, _: Cons<B1, S1>) -> Self::Output {
		Cons(PhantomData)
	}
}

impl<
	B1: Bit,
	B2: Bit + BitAnd<B1>,
	S1,
	S2: BitAnd<S1>
> BitAnd<Cons<B1, S1>> for Cons<B2, S2> {
	type Output = Cons<B2::Output, S2::Output>;

	fn bitand(self, _: Cons<B1, S1>) -> Self::Output {
		Cons(PhantomData)
	}
}

#[test]
fn test() {
	// -> 1 0 0 1 1
	let v1: Cons<Bit1, Cons<Bit0, Cons<Bit0, Cons<Bit1, Bit1>>>> = Default::default();
	// -> 1 1 1 1 0
	let v2: Cons<Bit1, Cons<Bit1, Cons<Bit1, Cons<Bit1, Bit0>>>> = Default::default();

	let _: Cons<Bit1, Cons<Bit0, Cons<Bit0, Cons<Bit1, Bit0>>>> = v1 & v2;
	let _: Cons<Bit1, Cons<Bit1, Cons<Bit1, Cons<Bit1, Bit1>>>> = v1 | v2;

	let _v4: <<Bit0 as ShiftRaising>::Output as Push<Bit1>>::Output = Default::default();
}
