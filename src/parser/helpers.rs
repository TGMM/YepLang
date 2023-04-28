#[macro_export]
macro_rules! tag_token (
    ($func_name:ident, $tag: expr) => (
        pub(crate) fn $func_name<'i>(tokens: Tokens<'i>) -> ParseRes<'i, Tokens<'i>> {
            verify(take(1usize), |t: &Tokens| t.tok_span[0].token == $tag)(tokens)
        }
    )
  );

#[cfg(test)]
pub(crate) mod test {
    use crate::{lexer::Token, parser::token::TokenSpan};

    #[macro_export]
    macro_rules! span_token {
        ($token:expr) => {
            TokenSpan {
                token: Ok($token),
                span: 1..1,
            }
        };
    }

    pub(crate) fn span_token_vec(tokens: Vec<Token>) -> Vec<TokenSpan> {
        tokens
            .into_iter()
            .map(|t| span_token!(t))
            .collect::<Vec<_>>()
    }
}
