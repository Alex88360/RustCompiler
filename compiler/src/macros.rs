#[macro_export]
macro_rules! try_consume_token {
    ($self:tt, $($inner:expr),*) => {
        match $self.peek() {
            token_kind => {
                if try_consume_token!(impl token_kind, $($inner),*) {
                    let tmp: TokenKind = token_kind;
                    $self.next();
                    Some(tmp)
                } else {
                    None
                }
            },
            _ => None
        }
    };
    (impl, ) => (false);
    (impl $token_kind:expr, $symbol:expr) => ($token_kind==$symbol);
    (impl $token_kind:expr, $symbol:expr, $($rest:expr),+) => (try_consume_token!(impl $token_kind, $symbol) || try_consume_token!(impl $token_kind, $($rest),*));
}

