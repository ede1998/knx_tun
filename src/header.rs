use nom_derive::{NomBE, Parse};
use rusticata_macros::newtype_enum;

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ServiceType(u16);

newtype_enum! {
    impl debug ServiceType {
    SearchRequest = 0x0201,
    SearchResponse = 0x0202,
    DescriptionRequest = 0x0203,
    DescriptionResponse = 0x0204,
    ConnectRequest = 0x0205,
    ConnectResponse = 0x0206,
    ConnectionstateRequest = 0x0207,
    ConnectionstateResponse = 0x0208,
    DisconnectRequest = 0x0209,
    DisconnectResponse = 0x020A,
    TunnelRequest = 0x0420,
    TunnelResponse = 0x0421,
    DeviceConfigurationRequest = 0x0310,
    DeviceConfigurationAck = 0x0311,
    RoutingIndication = 0x0530,
    RoutingLostMessage = 0x531,
}
}

impl From<ServiceType> for u16 {
    fn from(s: ServiceType) -> Self {
        s.0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ProtocolVersion(u8);

newtype_enum! {
    impl debug ProtocolVersion {
    V1_0 = 0x10,
}
}

impl From<ProtocolVersion> for u8 {
    fn from(pv: ProtocolVersion) -> Self {
        pv.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Header {
    pub version: ProtocolVersion,
    pub service_type: ServiceType,
    pub body_length: u16,
}

impl Header {
    pub const fn new(service_type: ServiceType, body_length: u16) -> Self {
        Self {
            version: ProtocolVersion::V1_0,
            service_type,
            body_length,
        }
    }

    pub const LENGTH: u16 = 0x06;
}

pub mod parse {
    use super::*;
    use crate::parse_helper::length_data_incl;
    use nom::{
        combinator::{map, verify},
        number::streaming::{be_u16, be_u8},
        IResult,
    };

    pub(crate) fn header(i: &[u8]) -> IResult<&[u8], Header> {
        let (i, inner) = length_data_incl(be_u8)(i)?;
        let header_len = 1 + inner.len() as u16;
        let (inner, _) =
            verify(ProtocolVersion::parse, |&p| p == ProtocolVersion::V1_0)(inner)?;
        let (inner, service_type) = ServiceType::parse(inner)?;
        let (_, body_length) = map(be_u16, |total_length| total_length - header_len)(inner)?;
        Ok((i, Header::new(service_type, body_length)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_header() {
        let serialized = vec![0x06, 0x10, 0x02, 0x03, 0x12, 0x34 + 0x06];
        let (remainder, result) = parse::header(&serialized).unwrap();

        assert!(remainder.is_empty(), "Output was not consumed entirely.");
        let expected = Header::new(ServiceType::DescriptionRequest, 0x1234);
        assert_eq!(expected, result);
    }
}
