use std::marker::PhantomData;

use fingertrees::sync::FingerTree;
use fingertrees::Measured;

use crate::{
    debruijn::DeBruijn,
    num::{pred::Pred, zero::Zero},
    type_nat::{TypeNat, S, Z},
};

pub struct Context<N, V>
where
    N: TypeNat,
    V: Measured,
{
    pub(crate) ft: FingerTree<V>,
    pub(crate) pd: PhantomData<N>,
}

impl<N, V> IntoIterator for Context<N, V>
where
    N: TypeNat,
    V: Measured,
{
    type Item = V;

    type IntoIter = <FingerTree<V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.ft.into_iter()
    }
}

impl<N, V> Context<S<N>, V>
where
    N: TypeNat,
    V: Measured,
{
    pub fn lookup<Ix>(&self, db: DeBruijn<S<N>, Ix>) -> V
    where
        Ix: Zero + Pred<Output = Ix>,
    {
        let mut ix: Ix = db.ix;
        let mut vw: Option<(V, Context<S<N>, V>)> = self.view();
        while !ix.is_zero() {
            ix = ix.pred();
            vw = vw.unwrap().1.view();
        }
        vw.unwrap().0
    }
}

impl<N, V> Context<N, V>
where
    N: TypeNat,
    V: Measured,
{
    pub fn new() -> Context<Z, V> {
        Context {
            ft: FingerTree::new(),
            pd: PhantomData::<Z>,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.ft.is_empty()
    }

    pub fn push(&self, value: V) -> Context<S<N>, V> {
        Context {
            ft: self.ft.push_right(value),
            pd: PhantomData::<S<N>>,
        }
    }

    pub fn view(&self) -> Option<(V, Context<N, V>)> {
        let (hd, tl) = self.ft.view_right()?;
        Some((
            hd,
            Context {
                ft: tl,
                pd: PhantomData::<N>,
            },
        ))
    }
}
