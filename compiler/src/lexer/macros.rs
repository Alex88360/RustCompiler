#[macro_export]
macro_rules! try_consume_char {
    ($self:tt, $($inner:tt),*) => {
        if let Some(c) = $self.chars.peek() {
            if try_consume_char!(impl c, $($inner),*) {
                let tmp = *c;
                $self.consume_char();
                Some(tmp)
            } else {
                None
            }
        } else {
            None
        }

    };
    (impl, ) => (false);
    (impl $c:tt, $symbol:tt) => (*$c==$symbol);
    (impl $c:tt, $symbol:tt, $($rest:tt),+) => (try_consume_char!(impl $c, $symbol) || try_consume_char!(impl $c, $($rest),*));
}