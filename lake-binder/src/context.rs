use std::marker::PhantomData;

use fingertrees::sync::FingerTree;
use fingertrees::Measured;

use crate::{
    debruijn::DeBruijn,
    num::{pred::Pred, zero::Zero},
    type_nat::{TypeNat, S, Z},
};

pub struct Context<N: TypeNat, V: Measured>(pub(crate) FingerTree<V>, pub(crate) PhantomData<N>);

impl<N, V> IntoIterator for Context<N, V>
where
    N: TypeNat,
    V: Measured,
{
    type Item = V;

    type IntoIter = <FingerTree<V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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
        let mut ix: Ix = db.0;
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
        Context(FingerTree::new(), PhantomData::<Z>)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn push(&self, value: V) -> Context<S<N>, V> {
        Context(self.0.push_right(value), PhantomData::<S<N>>)
    }

    pub fn view(&self) -> Option<(V, Context<N, V>)> {
        let (hd, tl) = self.0.view_right()?;
        Some((hd, Context(tl, PhantomData::<N>)))
    }
}
