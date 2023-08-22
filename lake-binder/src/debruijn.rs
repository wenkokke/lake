use std::marker::PhantomData;

use crate::{
    num::{nat::Nat, one::One, pred::Pred, succ::Succ, zero::Zero},
    type_nat::{TypeNat, S},
};

pub type DefaultIx = u32;

#[derive(Eq, PartialEq)]
pub struct DeBruijn<N: TypeNat, Ix = DefaultIx> {
    pub(crate) ix: Ix,
    pub(crate) pd: PhantomData<N>,
}

impl<N, Ix> Zero for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: Zero,
{
    fn zero() -> DeBruijn<S<N>, Ix> {
        DeBruijn {
            ix: <Ix as Zero>::zero(),
            pd: PhantomData::<S<N>>,
        }
    }

    fn is_zero(&self) -> bool {
        self.ix.is_zero()
    }
}

impl<N, Ix> One for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: One,
{
    fn one() -> DeBruijn<S<N>, Ix> {
        DeBruijn {
            ix: <Ix as One>::one(),
            pd: PhantomData::<S<N>>,
        }
    }
}

impl<N, Ix> Succ for DeBruijn<N, Ix>
where
    N: TypeNat,
    Ix: Succ<Output = Ix>,
{
    type Output = DeBruijn<S<N>, Ix>;

    fn succ(self) -> Self::Output {
        DeBruijn {
            ix: self.ix.succ(),
            pd: PhantomData::<S<N>>,
        }
    }
}

impl<N, Ix> Pred for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: Pred<Output = Option<Ix>>,
{
    type Output = DeBruijn<N, Ix>;

    fn pred(self) -> Self::Output {
        DeBruijn {
            ix: self.ix.pred().unwrap(),
            pd: PhantomData::<N>,
        }
    }
}

impl<N, Ix> Nat for DeBruijn<S<N>, Ix>
where
    N: TypeNat,
    Ix: Zero + One + Succ<Output = Ix> + Pred<Output = Option<Ix>>,
{
    type Succ = DeBruijn<S<S<N>>, Ix>;
    type Pred = DeBruijn<N, Ix>;
}
