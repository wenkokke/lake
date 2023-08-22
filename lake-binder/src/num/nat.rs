use super::{one::One, pred::Pred, succ::Succ, zero::Zero};

pub trait Nat: Zero + One + Succ<Output = Self::Succ> + Pred<Output = Self::Pred> {
    type Succ;
    type Pred;
}

macro_rules! nat_impl {
    ($t:ty) => {
        impl Nat for $t {
            type Succ = $t;
            type Pred = Option<$t>;
        }
    };
}

nat_impl!(usize);
nat_impl!(u8);
nat_impl!(u16);
nat_impl!(u32);
nat_impl!(u64);
#[cfg(has_i128)]
nat_impl!(u128);

nat_impl!(isize);
nat_impl!(i8);
nat_impl!(i16);
nat_impl!(i32);
nat_impl!(i64);
#[cfg(has_i128)]
nat_impl!(i128);

nat_impl!(f32);
nat_impl!(f64);
