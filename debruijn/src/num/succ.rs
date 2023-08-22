use crate::num::one::One;

/// Defines the successor function for `Self`.
pub trait Succ: Sized {
    type Output;

    /// Returns the successor of `Self`, `self + 1`.
    fn succ(self) -> Self::Output;
}

macro_rules! succ_impl {
    ($t:ty) => {
        impl Succ for $t {
            type Output = $t;

            #[inline]
            fn succ(self) -> $t {
                self + <$t as One>::one()
            }
        }
    };
}

succ_impl!(usize);
succ_impl!(u8);
succ_impl!(u16);
succ_impl!(u32);
succ_impl!(u64);
#[cfg(has_i128)]
succ_impl!(u128);

succ_impl!(isize);
succ_impl!(i8);
succ_impl!(i16);
succ_impl!(i32);
succ_impl!(i64);
#[cfg(has_i128)]
succ_impl!(i128);

succ_impl!(f32);
succ_impl!(f64);
