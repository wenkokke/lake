use std::{marker::PhantomData, ops::Add};

use crate::num::constant::Constant;
use crate::num::one::One;
use crate::num::zero::Zero;

pub trait TypeNat: 'static {}

pub struct Z {}

impl TypeNat for Z {}

impl<T> Constant<Z, T> for Z
where
    T: Sized + Zero,
{
    #[inline]
    fn value() -> T {
        <T as Zero>::zero()
    }
}
pub struct S<N: TypeNat>(pub PhantomData<N>);

impl<N: TypeNat> TypeNat for S<N> {}

impl<N, T> Constant<S<N>, T> for S<N>
where
    N: TypeNat + Constant<N, T>,
    T: Sized + One + Add<T, Output = T>,
{
    #[inline]
    fn value() -> T {
        <N as Constant<N, T>>::value() + <T as One>::one()
    }
}
