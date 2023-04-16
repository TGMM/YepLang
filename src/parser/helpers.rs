#[macro_export]
macro_rules! span_token {
    ($token:expr) => {
        ($token, SimpleSpan::new(1usize, 1usize))
    };
}
