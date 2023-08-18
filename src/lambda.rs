#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Name {
    Name(String),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Term {
    Var(Name),
    Lam(Name, Box<Self>),
    App(Box<Self>, Box<Self>),
}

#[cfg(test)]
mod tests {
    use crate::lambda::{Name, Term};

    #[test]
    fn show_identity() {
        let term = Term::Lam(
            Name::Name(String::from("x")),
            Box::new(Term::Var(Name::Name(String::from("x")))),
        );
        assert_eq!("Lam(Name(\"x\"), Var(Name(\"x\")))", format!("{term:?}"));
    }
}
