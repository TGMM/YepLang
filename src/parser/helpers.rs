#[macro_export]
macro_rules! recursive_parser {
    ($global_name:ident,
        $fn_name:ident,
        $out_ty:ty,
        declarations { $($decl:stmt)* },
        main_definition $main_def:block,
        definitions $def:block ) => {
        pub(crate) static $global_name: GlobalParser<'static, $out_ty> =
            LazyLock::new(|| Arc::new(RwLock::new(Recursive::declare())));

        pub(crate) fn $fn_name() -> RecursiveParser<'static, $out_ty> {
            let mut parser = $global_name.read().unwrap().clone();

            // Declarations
            $($decl)*

            // Main definition
            if !parser.is_defined() {
                let parser_def = $main_def;
                parser.define(parser_def);
            }

            // Definitions
            $def

            parser
        }
    };
}

#[cfg(test)]
pub(crate) mod test {
    use crate::{
        ast::{BOp, BOpType, Id},
        lexer::Token,
    };
    use chumsky::{
        input::{SpannedInput, Stream},
        prelude::Input,
        span::SimpleSpan,
    };
    use std::vec::IntoIter;

    #[macro_export]
    macro_rules! span_token {
        ($token:expr) => {
            ($token, SimpleSpan::from(0usize..0))
        };
    }

    pub(crate) fn span_token_vec(tokens: Vec<Token>) -> Vec<(Token, SimpleSpan)> {
        tokens
            .into_iter()
            .map(|t| span_token!(t))
            .collect::<Vec<_>>()
    }

    pub(crate) fn stream_token_vec(
        tokens: Vec<Token>,
    ) -> SpannedInput<Token, SimpleSpan, Stream<IntoIter<(Token, SimpleSpan)>>> {
        let tokens = span_token_vec(tokens);
        let length = tokens.len();
        Stream::from_iter(tokens).spanned((length..length).into())
    }

    impl From<BOpType> for BOp {
        fn from(bop_type: BOpType) -> Self {
            BOp {
                bop_type,
                span: SimpleSpan::new(0, 0),
            }
        }
    }

    impl From<&str> for Id {
        fn from(value: &str) -> Self {
            Id {
                id_str: value.to_string(),
                span: SimpleSpan::new(0, 0),
            }
        }
    }
}
