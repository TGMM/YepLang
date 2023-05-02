// #[cfg(test)]
// pub(crate) mod test {
//     use crate::{lexer::Token, parser::token::TokenSpan};

//     #[macro_export]
//     macro_rules! span_token {
//         ($token:expr) => {
//             TokenSpan {
//                 token: Ok($token),
//                 span: 1..1,
//             }
//         };
//     }

//     pub(crate) fn span_token_vec(tokens: Vec<Token>) -> Vec<TokenSpan> {
//         tokens
//             .into_iter()
//             .map(|t| span_token!(t))
//             .collect::<Vec<_>>()
//     }
// }
