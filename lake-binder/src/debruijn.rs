use std::marker::PhantomData;

use crate::{
    num::{one::One, pred::Pred, succ::Succ, zero::Zero},
    type_nat::{TypeNat, S},
};

pub type DefaultIx = u32;

#[derive(Eq, PartialEq)]
pub struct DeBruijn<N: TypeNat, Ix = DefaultIx>(pub(crate) Ix, pub(crate) PhantomData<N>);

impl<N, Ix> Zero for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: Zero,
{
    fn zero() -> DeBruijn<S<N>, Ix> {
        DeBruijn(<Ix as Zero>::zero(), PhantomData::<S<N>>)
    }

    fn is_zero(&self) -> bool {
        self.0.is_zero()
    }
}

impl<N, Ix> One for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: One,
{
    fn one() -> DeBruijn<S<N>, Ix> {
        DeBruijn(<Ix as One>::one(), PhantomData::<S<N>>)
    }
}

impl<N, Ix> Succ for DeBruijn<N, Ix>
where
    N: TypeNat,
    Ix: Succ<Output = Ix>,
{
    type Output = DeBruijn<S<N>, Ix>;

    fn succ(self) -> Self::Output {
        DeBruijn(self.0.succ(), PhantomData::<S<N>>)
    }
}

impl<N, Ix> Pred for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: Pred<Output = Option<Ix>>,
{
    type Output = DeBruijn<N, Ix>;

    fn pred(self) -> Self::Output {
        DeBruijn(self.0.pred().unwrap(), PhantomData::<N>)
    }
}
