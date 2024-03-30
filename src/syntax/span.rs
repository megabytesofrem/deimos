use super::lexer::SourceLoc;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub target: T,
    pub location: SourceLoc,
}

#[macro_export]
macro_rules! spanned {
    ($target:expr, $loc:expr) => {
        Spanned {
            target: $target,
            location: $loc,
        }
    };
}
