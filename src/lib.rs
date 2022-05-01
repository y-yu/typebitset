use core::fmt;
use core::fmt::{Debug, Display};
use core::hash::Hash;
use core::marker::PhantomData;
use core::ops::{BitAnd, BitOr};

pub trait Bitset: Copy + Clone + Default + Eq + PartialEq + Debug + Hash {}

/// Implementation of bitset. See [`Set`]
#[derive(Copy, Clone, Default, Eq, PartialEq, Debug, Hash)]
pub struct Cons<B, S: Bitset>(PhantomData<(B, S)>);

impl<B: Display + Default, S: Display + Default + Bitset> Display for Cons<B, S> {
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

impl<B: Bit, S: Positive + Bitset> Set for Cons<B, S> {
	const N: usize = <S as Set>::N * 2 + 1;
}

/// A trait implemented if the bitset is positive (not zero).
pub trait Positive: Set {}
impl Positive for Bit1 {}
impl<B: Bit, S: Positive + Bitset> Positive for Cons<B, S> {}

/// A trait which represents a bit.
pub trait Bit: Set {}
impl Bit for Bit0 {}
impl Bit for Bit1 {}
impl<B: Bit> Bitset for B { }
impl<B: Bit, S: Bitset> Bitset for Cons<B, S> { }

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

impl<B: Bit, S: Bitset> ShiftRaising for Cons<B, S>
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

impl<B: Bit, S: Bitset> ShiftLowering for Cons<B, S>
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

impl<B0: Bit, B: Bit, S: Positive + Bitset> Push<B0> for Cons<B, S> {
	type Output = Cons<B0, Cons<B, S>>;
}

macro_rules! impl_binary_op {
	($($bita:ident, $bitb:ident, $bito_and:ident, $bito_or:ident;)*) => {
		$(
			impl BitAnd<$bita> for $bitb {
				type Output = $bito_and;
				fn bitand(self, _: $bita) -> Self::Output {
					$bito_and
				}
			}

			impl BitOr<$bita> for $bitb {
				type Output = $bito_or;
				fn bitor(self, _: $bita) -> Self::Output {
					$bito_or
				}
			}

			impl<Sa: Bitset> BitAnd<Cons<$bita, Sa>> for $bitb
			where
				Cons<$bita, Sa>: Set
			{
				type Output = $bito_and;
				fn bitand(self, _: Cons<$bita, Sa>) -> Self::Output {
					$bito_and
				}
			}

			impl<Sb: Bitset> BitAnd<$bita> for Cons<$bitb, Sb>
			where
				Cons<$bitb, Sb>: Set
			{
				type Output = $bito_and;
				fn bitand(self, _: $bita) -> Self::Output {
					$bito_and
				}
			}

			impl<Sa: Bitset> BitOr<Cons<$bita, Sa>> for $bitb
			where
				Cons<$bita, Sa>: Set,
				Sa: Push<$bito_or>,
			{
				type Output = <Sa as Push<$bito_or>>::Output;
				fn bitor(self, _: Cons<$bita, Sa>) -> Self::Output {
					<Self::Output as Default>::default()
				}
			}

			impl<Sb: Bitset> BitOr<$bita> for Cons<$bitb, Sb>
			where
				Cons<$bitb, Sb>: Set,
				Sb: Push<$bito_or>,
			{
				type Output = <Sb as Push<$bito_or>>::Output;
				fn bitor(self, _: $bita) -> Self::Output {
					<Self::Output as Default>::default()
				}
			}

			impl<Sa: Bitset, Sb: Bitset> BitAnd<Cons<$bita, Sa>> for Cons<$bitb, Sb>
			where
				Cons<$bita, Sa>: Set,
				Cons<$bitb, Sb>: Set,
				Sb: BitAnd<Sa>,
				<Sb as BitAnd<Sa>>::Output: Push<$bito_and>,
			{
				type Output = <<Sb as BitAnd<Sa>>::Output as Push<$bito_and>>::Output;
				fn bitand(self, _rhs: Cons<$bita, Sa>) -> Self::Output {
					<Self::Output as Default>::default()
				}
			}

			impl<Sa: Bitset, Sb: Bitset> BitOr<Cons<$bita, Sa>> for Cons<$bitb, Sb>
			where
				Cons<$bita, Sa>: Set,
				Cons<$bitb, Sb>: Set,
				Sb: BitOr<Sa>,
				<Sb as BitOr<Sa>>::Output: Push<$bito_or>,
			{
				type Output = <<Sb as BitOr<Sa>>::Output as Push<$bito_or>>::Output;
				fn bitor(self, _rhs: Cons<$bita, Sa>) -> Self::Output {
					<Self::Output as Default>::default()
				}
			}
		)*
	}
}

impl_binary_op! {
	// Lhs, Rhs, And, Or
	Bit0, Bit0, Bit0, Bit0;
	Bit1, Bit0, Bit0, Bit1;
	Bit0, Bit1, Bit0, Bit1;
	Bit1, Bit1, Bit1, Bit1;
}

// pub trait FromNumImpl<const N: usize> {
// 	type Output;
// }
//
// pub type FromNum<const N: usize> = <Bit0 as FromNumImpl<N>>::Output;
//
// impl FromNumImpl<0> for Bit0 {
// 	type Output = Bit0;
// }
//
// macro_rules! impl_set_of {
// 	(@out ) => { $crate::Bit0 };
// 	(@out 0 $(,$xs:expr)*) => {
// 		$crate::Cons<$crate::Bit0, impl_set_of!(@out $($xs),*)>
// 	};
// 	(@out 1 $(,$xs:expr)*) => {
// 		$crate::Cons<$crate::Bit1, impl_set_of!(@out $($xs),*)>
// 	};
// 	(@imp $($xs:expr),*) => {
// 		impl FromNumImpl<{impl_set_of!(@bittonum $($xs),*)}> for Bit0 {
// 			type Output = impl_set_of!(@out $($xs),*);
// 		}
// 	};
// 	(@bittonum ) => {0usize};
// 	(@bittonum $x0:expr $(,$xs:expr)*) => {
// 		2usize * (impl_set_of!(@bittonum $($xs),*)) + $x0
// 	};
// 	(@ $($ys:expr),* ; ) => {
// 		impl_set_of!(@imp $($ys,)* 1);
// 	};
// 	(@ $($ys:expr),* ; 1 $(,$xs:expr)*) => {
// 		log_syntax!(@ $($ys,)* 1 ; $($xs),*);
// 		impl_set_of!(@ $($ys,)* 1 ; $($xs),*);
// 		impl_set_of!(@ $($ys,)* 0 ; $($xs),*);
// 	};
// 	() => {
// 		impl_set_of!(@imp );
// 	};
// 	(1 $(,$xs:expr)*) => {
// 		log_syntax!($($xs),*);
// 		impl_set_of!(@ ; 1 $(,$xs)*);
// 		impl_set_of!($($xs),*);
// 	}
// }

#[test]
fn test() {
	let v1: Cons<Bit1, Cons<Bit0, Bit1>> = Default::default();
	let v2: Cons<Bit1, Bit1> = Default::default();
	let _: Bit1 = v1 & v2;
	let _: Cons<Bit1, Cons<Bit1, Bit1>> = v1 | v2;
	let _v4: <<Bit0 as ShiftRaising>::Output as Push<Bit1>>::Output = Default::default();

	let v5: Cons<Cons<Bit0, Bit1>, bool> = Default::default();
	let v6: Cons<Cons<Bit0, Bit1>, Cons<Bit0, Bit1>> = Default::default();
}
