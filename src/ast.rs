use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum Name {
    Name(String),
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum AST {
    Var(Name),
    Lam(Name, Box<Self>),
    App(Box<Self>, Box<Self>),
}

#[cfg(test)]
mod tests {
    use crate::ast::{Name, AST};

    #[test]
    fn show_identity() {
        let term = AST::Lam(
            Name::Name(String::from("x")),
            Box::new(AST::Var(Name::Name(String::from("x")))),
        );
        assert_eq!("Lam(Name(\"x\"), Var(Name(\"x\")))", format!("{term:?}"));
    }
}
