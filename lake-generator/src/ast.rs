use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum AST {
    Var(i32),
    Lam(Box<Self>),
    App(Box<Self>, Box<Self>),
}
