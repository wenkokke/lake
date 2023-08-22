use std::marker::PhantomData;

pub trait TypeNat: 'static {}

pub struct Z();

impl TypeNat for Z {}

pub struct S<N: TypeNat>(PhantomData<N>);

impl<N: TypeNat> TypeNat for S<N> {}
