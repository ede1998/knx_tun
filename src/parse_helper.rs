use nom::{IResult, InputLength, InputTake, Parser, ToUsize, combinator::map, multi::length_data};
use nom::error::ParseError;

/// Wraps `length_data`. Gets a number N from the parser and returns a
/// subslice of the input of that size. Differs from `length_data` because
/// the size of N itself is included in the value of N.
pub fn length_data_incl<I, N, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, I, E>
where
  I: InputLength + InputTake,
  N: ToUsize,
  F: Parser<I, N, E>,
  E: ParseError<I>,
{
    length_data(map(f, |len| len.to_usize() - std::mem::size_of::<N>()))
}
