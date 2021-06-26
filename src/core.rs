use cookie_factory::BackToTheBuffer;
use nom_derive::NomBE;

use crate::connect::{ConnectRequest, ConnectResponse};
use crate::disconnect::{DisconnectRequest, DisconnectResponse};
use crate::keep_alive::{ConnectionStateRequest, ConnectionStateResponse};
use crate::snack::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u16)]
pub enum ServiceType {
    SearchRequest = 0x0201,
    SearchResponse = 0x0202,
    DescriptionRequest = 0x0203,
    DescriptionResponse = 0x0204,
    ConnectRequest = 0x0205,
    ConnectResponse = 0x0206,
    ConnectionStateRequest = 0x0207,
    ConnectionStateResponse = 0x0208,
    DisconnectRequest = 0x0209,
    DisconnectResponse = 0x020A,
    TunnelRequest = 0x0420,
    TunnelResponse = 0x0421,
    DeviceConfigurationRequest = 0x0310,
    DeviceConfigurationAck = 0x0311,
    RoutingIndication = 0x0530,
    RoutingLostMessage = 0x531,
}

impl From<ServiceType> for u16 {
    fn from(f: ServiceType) -> Self {
        f as u16
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum ProtocolVersion {
    V1_0 = 0x10,
}

impl From<ProtocolVersion> for u8 {
    fn from(f: ProtocolVersion) -> Self {
        f as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
struct Header {
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

    const LENGTH: u16 = 0x06;

    pub(crate) fn parse<'a>(i: &'a [u8]) -> IResult<'a, Self> {
        use nm::*;
        context(stringify!(Header), |i: &'a [u8]| {
            let (i, inner) = length_data_incl(be_u8)(i)?;
            let header_len = 1 + inner.len() as u16;
            let (inner, _) =
                verify(ProtocolVersion::parse, |&p| p == ProtocolVersion::V1_0)(inner)?;
            let (inner, service_type) = ServiceType::parse(inner)?;
            let (_, body_length) = map(be_u16, |total_length| total_length - header_len)(inner)?;
            Ok((i, Header::new(service_type, body_length)))
        })(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        length_data_incl(
            1,
            tuple((
                be_u8(self.version.into()),
                be_u16(self.service_type.into()),
                be_u16(self.body_length + Header::LENGTH),
            )),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum Body {
    ConnectRequest(ConnectRequest),
    ConnectResponse(ConnectResponse),
    DisconnectRequest(DisconnectRequest),
    DisconnectResponse(DisconnectResponse),
    ConnectionStateRequest(ConnectionStateRequest),
    ConnectionStateResponse(ConnectionStateResponse),
}

impl Body {
    fn as_service_type(&self) -> ServiceType {
        match self {
            Self::ConnectRequest(_) => ServiceType::ConnectRequest,
            Self::ConnectResponse(_) => ServiceType::ConnectResponse,
            Self::DisconnectRequest(_) => ServiceType::DisconnectRequest,
            Self::DisconnectResponse(_) => ServiceType::DisconnectResponse,
            Self::ConnectionStateRequest(_) => ServiceType::ConnectionStateRequest,
            Self::ConnectionStateResponse(_) => ServiceType::ConnectionStateResponse,
        }
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        move |buf| match self {
            Body::ConnectRequest(m) => m.gen()(buf),
            Body::ConnectResponse(m) => m.gen()(buf),
            Body::DisconnectRequest(m) => m.gen()(buf),
            Body::DisconnectResponse(m) => m.gen()(buf),
            Body::ConnectionStateRequest(m) => m.gen()(buf),
            Body::ConnectionStateResponse(m) => m.gen()(buf),
        }
    }

    pub(crate) fn parse(i: &[u8], service_type: ServiceType) -> IResult<Body> {
        use nm::*;
        context(
            "Body",
            all_consuming(|i| match service_type {
                ServiceType::ConnectRequest => into(ConnectRequest::parse)(i),
                ServiceType::ConnectResponse => into(ConnectResponse::parse)(i),
                ServiceType::DisconnectRequest => into(DisconnectRequest::parse)(i),
                ServiceType::DisconnectResponse => into(DisconnectResponse::parse)(i),
                ServiceType::ConnectionStateRequest => into(ConnectionStateRequest::parse)(i),
                ServiceType::ConnectionStateResponse => into(ConnectionStateResponse::parse)(i),
                _ => Err(Err::Error(make_error(i, NomErrorKind::Switch))),
            }),
        )(i)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Frame {
    pub body: Body,
}

impl Frame {
    pub fn wrap(body: Body) -> Self {
        Self { body }
    }

    pub fn gen<'a, W: BackToTheBuffer + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        back_to_the_buffer(
            Header::LENGTH as usize,
            move |buf: WriteContext<W>| gen(self.body.gen(), buf),
            move |buf, len| {
                gen_simple(
                    Header::new(self.body.as_service_type(), len as u16).gen(),
                    buf,
                )
            },
        )
    }

    pub fn parse(i: &[u8]) -> IResult<Self> {
        use nm::*;
        let (i, header) = Header::parse(i)?;
        let (i, inner) = take(header.body_length)(i)?;
        let (_, body) = Body::parse(inner, header.service_type)?;
        Ok((i, Frame { body }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_header() {
        let serialized = vec![0x06, 0x10, 0x02, 0x03, 0x12, 0x34 + 0x06];
        let (remainder, result) = Header::parse(&serialized).unwrap();

        assert!(remainder.is_empty(), "Output was not consumed entirely.");
        let expected = Header::new(ServiceType::DescriptionRequest, 0x1234);
        assert_eq!(expected, result);
    }

    #[test]
    fn gen_header() {
        let header = Header::new(ServiceType::DescriptionRequest, 0x1234);
        let (serialized, len) = cf::gen(header.gen(), vec![]).unwrap();

        println!("{:#x?}", serialized);
        println!("{:#x?}", len);
        assert_eq!(serialized.len(), 0x06, "Wrong length.");
        assert_eq!(len, 0x06, "Wrong length in result.");
        assert_eq!(serialized[0], 6, "Wrong header length.");
        assert_eq!(serialized[1], 0x10, "Wrong protocol version.");
        assert_eq!(serialized[2], 0x02, "Wrong service_type high value.");
        assert_eq!(serialized[3], 0x03, "Wrong service_type low value.");
        assert_eq!(serialized[4], 0x12, "Wrong total length high value.");
        assert_eq!(serialized[5], 0x34 + 0x06, "Wrong total length high value.");
    }

    #[test]
    fn parse_gen_header() {
        let serialized = vec![0x06, 0x10, 0x02, 0x03, 0x12, 0x34 + 0x06];
        let header = Header::parse(&serialized).unwrap().1;
        let re_serialized = cf::gen_simple(header.gen(), vec![]).unwrap();

        assert_eq!(&serialized[..], &re_serialized[..]);
    }
}
