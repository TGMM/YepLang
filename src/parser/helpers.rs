#[cfg(test)]
#[macro_export]
macro_rules! span_token {
    ($token:expr) => {
        ($token, SimpleSpan::new(1usize, 1usize))
    };
}

#[macro_export]
macro_rules! tag_token (
    ($func_name:ident, $tag: expr) => (
        pub(crate) fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            verify(take(1usize), |t: &Tokens| t.tok_span[0].token == $tag)(tokens)
        }
    )
  );
