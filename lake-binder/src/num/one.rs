/// Defines a multiplicative identity element for `Self`.
pub trait One: Sized {
    /// Returns the multiplicative identity element of `Self`, `1`.
    fn one() -> Self;
}

macro_rules! one_impl {
    ($t:ty, $v:expr) => {
        impl One for $t {
            #[inline]
            fn one() -> $t {
                $v
            }
        }
    };
}

one_impl!(usize, 1);
one_impl!(u8, 1);
one_impl!(u16, 1);
one_impl!(u32, 1);
one_impl!(u64, 1);
#[cfg(has_i128)]
one_impl!(u128, 1);

one_impl!(isize, 1);
one_impl!(i8, 1);
one_impl!(i16, 1);
one_impl!(i32, 1);
one_impl!(i64, 1);
#[cfg(has_i128)]
one_impl!(i128, 1);

one_impl!(f32, 1.0);
one_impl!(f64, 1.0);
