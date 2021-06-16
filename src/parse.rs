use std::net::{Ipv4Addr, SocketAddrV4};

use nom::bytes::complete::take;
use nom::combinator::verify;
use nom::error::{make_error, ErrorKind, ParseError};
use nom::number::complete::be_u32;
use nom::number::streaming::{be_u16, be_u8};
use nom::sequence::tuple;
use nom::{combinator::map, multi::length_data, InputLength, InputTake, Parser, ToUsize};
use nom::{Err, IResult};
use nom_derive::Parse;

use crate::core::*;
use crate::cri::{ConnectRequest, ConnectionRequestInformation, ConnectionType, KnxLayer, Tunnel};
use crate::hpai::{HostProtocolAddressInformation, HostProtocolCode};

/// Wraps `length_data`. Gets a number N from the parser and returns a
/// subslice of the input of that size. Differs from `length_data` because
/// the size of N itself is included in the value of N.
pub fn length_data_incl<I, N, E, F>(f: F) -> impl FnMut(I) -> IResult<I, I, E>
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

pub(crate) fn body(i: &[u8], service_type: ServiceType) -> IResult<&[u8], Body> {
    match service_type {
        ServiceType::ConnectRequest => map(connect_request, Body::ConnectRequest)(i),
        _ => Err(Err::Error(make_error(i, ErrorKind::Switch))),
    }
}

pub(crate) fn frame(i: &[u8]) -> IResult<&[u8], Frame> {
    let (i, header) = header(i)?;
    let (i, inner) = take(header.body_length)(i)?;
    let (inner, body) = body(inner, header.service_type)?;
    Ok((i, Frame { header, body }))
}

pub(crate) fn connect_request(i: &[u8]) -> IResult<&[u8], ConnectRequest> {
    map(
        tuple((
            host_protocol_address_information,
            host_protocol_address_information,
            connection_request_information,
        )),
        |(ctl, data, cri)| ConnectRequest {
            control_endpoint: ctl,
            data_endpoint: data,
            cri,
        },
    )(i)
}

pub(crate) fn host_protocol_address_information(
    i: &[u8],
) -> IResult<&[u8], HostProtocolAddressInformation> {
    let (i, protocol_code) = HostProtocolCode::parse(i)?;
    let (i, address) = map(tuple((be_u32, be_u16)), |(ip, port)| {
        SocketAddrV4::new(ip.into(), port)
    })(i)?;
    Ok((
        i,
        HostProtocolAddressInformation::new(protocol_code, address),
    ))
}

pub(crate) fn connection_request_information(
    i: &[u8],
) -> IResult<&[u8], ConnectionRequestInformation> {
    let (i, inner) = length_data_incl(be_u8)(i)?;
    let (inner, connection_type) = ConnectionType::parse(inner)?;
    match connection_type {
        ConnectionType::Tunnel => map(
            connection_request_information_tunnel,
            ConnectionRequestInformation::Tunnel,
        )(inner),
        _ => Err(Err::Error(make_error(i, ErrorKind::Switch))),
    }
    .map(|(_, cri)| (i, cri))
}

pub(crate) fn connection_request_information_tunnel(i: &[u8]) -> IResult<&[u8], Tunnel> {
    map(tuple((KnxLayer::parse, take(1usize))), |(layer, _)| {
        Tunnel { layer }
    })(i)
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
