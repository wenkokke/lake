use crate::num::one::One;
use crate::num::zero::Zero;

/// Defines the predecessor function for `Self`.
pub trait Pred: Sized {
    type Output;

    /// Returns the predecessor of `Self`, `self + 1`.
    fn pred(self) -> Self::Output;
}

macro_rules! pred_impl {
    ($t:ty) => {
        impl Pred for $t {
            type Output = Option<$t>;

            #[inline]
            fn pred(self) -> Option<$t> {
                if <$t as Zero>::is_zero(&self) {
                    None
                } else {
                    Some(self - <$t as One>::one())
                }
            }
        }
    };
}

pred_impl!(usize);
pred_impl!(u8);
pred_impl!(u16);
pred_impl!(u32);
pred_impl!(u64);
#[cfg(has_i128)]
pred_impl!(u128);

pred_impl!(isize);
pred_impl!(i8);
pred_impl!(i16);
pred_impl!(i32);
pred_impl!(i64);
#[cfg(has_i128)]
pred_impl!(i128);

pred_impl!(f32);
pred_impl!(f64);
