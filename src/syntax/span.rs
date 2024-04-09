use super::lexer::SourceLoc;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub target: T,
    pub location: SourceLoc,
}

pub fn spanned<T>(target: T, location: SourceLoc) -> Spanned<T> {
    Spanned { target, location }
}

impl<T> Spanned<T> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            target: f(self.target),
            location: self.location,
        }
    }

    pub fn flat_map<U, F: FnOnce(T) -> Spanned<U>>(self, f: F) -> Spanned<U> {
        let spanned = f(self.target);
        Spanned {
            target: spanned.target,
            location: self.location,
        }
    }

    pub fn flat_map_spanned<U, E, F: FnOnce(T) -> Result<U, E>>(self, f: F) -> Result<Spanned<U>, E> {
        let spanned = f(self.target)?;
        Ok(Spanned {
            target: spanned,
            location: self.location,
        })
    }
}