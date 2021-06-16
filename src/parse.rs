use nom::combinator::verify;
use nom::error::ParseError;
use nom::number::streaming::{be_u16, be_u8};
use nom::{combinator::map, multi::length_data, IResult, InputLength, InputTake, Parser, ToUsize};
use nom_derive::Parse;

use crate::header::*;

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

pub(crate) fn header(i: &[u8]) -> IResult<&[u8], Header> {
    let (i, inner) = length_data_incl(be_u8)(i)?;
    let header_len = 1 + inner.len() as u16;
    let (inner, _) = verify(ProtocolVersion::parse, |&p| p == ProtocolVersion::V1_0)(inner)?;
    let (inner, service_type) = ServiceType::parse(inner)?;
    let (_, body_length) = map(be_u16, |total_length| total_length - header_len)(inner)?;
    Ok((i, Header::new(service_type, body_length)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_header() {
        let serialized = vec![0x06, 0x10, 0x02, 0x03, 0x12, 0x34 + 0x06];
        let (remainder, result) = super::header(&serialized).unwrap();

        assert!(remainder.is_empty(), "Output was not consumed entirely.");
        let expected = Header::new(ServiceType::DescriptionRequest, 0x1234);
        assert_eq!(expected, result);
    }
}
