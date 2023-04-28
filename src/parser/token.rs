use crate::lexer::Token;
use nom::{Input, InputLength, Needed};
use std::iter::Enumerate;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TokenSpan<'a> {
    pub token: Result<Token<'a>, ()>,
    pub span: Range<usize>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub(crate) struct Tokens<'a> {
    pub tok_span: &'a [TokenSpan<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [TokenSpan<'a>]) -> Self {
        Tokens {
            tok_span: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok_span.len()
    }
}

impl<'a> Input for Tokens<'a> {
    // InputTake
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok_span: &self.tok_span[0..count],
            start: self.start,
            end: self.end + count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok_span.split_at(count);
        let first = Tokens {
            tok_span: prefix,
            start: self.start,
            end: self.start + count - 1,
        };
        let second = Tokens {
            tok_span: suffix,
            start: self.start + count,
            end: self.end,
        };
        (second, first)
    }

    // InputLength
    #[inline]
    fn input_len(&self) -> usize {
        self.tok_span.len()
    }

    // InputIter
    type Item = &'a TokenSpan<'a>;
    type Iter = std::slice::Iter<'a, TokenSpan<'a>>;
    type IterIndices = Enumerate<Self::Iter>;

    #[inline]
    fn iter_indices(&self) -> Self::IterIndices {
        self.tok_span.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> Self::Iter {
        self.tok_span.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok_span.iter().position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok_span.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }

    // Slice?
    fn take_from(&self, index: usize) -> Self {
        let tok = &self.tok_span[index..];
        Tokens {
            tok_span: tok,
            start: self.start + index,
            end: self.end,
        }
    }
}
