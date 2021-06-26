use nom_derive::NomBE;

use crate::core::Body;
use crate::hpai::Hpai;
use crate::snack::*;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectionStateRequest {
    pub communication_channel_id: u8,
    pub control_endpoint: Hpai,
}

impl From<ConnectionStateRequest> for Body {
    fn from(f: ConnectionStateRequest) -> Self {
        Body::ConnectionStateRequest(f)
    }
}

impl ConnectionStateRequest {
    pub const fn new(communication_channel_id: u8, control_endpoint: Hpai) -> Self {
        Self {
            communication_channel_id,
            control_endpoint,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(ConnectionStateRequest),
            map(tuple((be_u8, be_u8, Hpai::parse)), |(chan, _, hpai)| {
                Self::new(chan, hpai)
            }),
        )(i)
    }
    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((
            be_u8(self.communication_channel_id),
            be_u8(0), // reserved
            self.control_endpoint.gen(),
        ))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum ConnectionState {
    /// The connection state is normal.
    NoError = 0x00,
    /// The KNXnet/IP Server device cannot find an active data connection with the specified ID.
    InvalidConnectionId = 0x21,
    /// The KNXnet/IP Server device detects an error concerning the data connection with the specified ID.
    DataConnectionError = 0x26,
    /// The KNXnet/IP Server device detects an error concerning the KNX subnetwork connection with the specified ID.
    KnxConnectionError = 0x27,
}

impl From<ConnectionState> for u8 {
    fn from(f: ConnectionState) -> Self {
        f as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectionStateResponse {
    pub communication_channel_id: u8,
    pub state: ConnectionState,
}

impl From<ConnectionStateResponse> for Body {
    fn from(f: ConnectionStateResponse) -> Self {
        Body::ConnectionStateResponse(f)
    }
}

impl ConnectionStateResponse {
    pub const fn new(communication_channel_id: u8, state: ConnectionState) -> Self {
        Self {
            communication_channel_id,
            state,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(ConnectionStateResponse),
            map(tuple((be_u8, ConnectionState::parse)), |(chan, state)| {
                Self::new(chan, state)
            }),
        )(i)
    }
    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((
            be_u8(self.communication_channel_id),
            be_u8(self.state.into()),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hpai::HostProtocolCode;

    const TEST_DATA_REQUEST: [u8; 10] = [
        0x15, // communication channel
        0x00, // reserved
        0x08, 0x01, 192, 168, 200, 12, 0xC3, 0xB4, // hpai
    ];

    fn make_test_request() -> ConnectionStateRequest {
        ConnectionStateRequest::new(
            21,
            Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 12], 50100),
        )
    }

    #[test]
    fn parse_connection_state_request() {
        let (rem, actual) = ConnectionStateRequest::parse(&TEST_DATA_REQUEST).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_request();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_connection_state_request() {
        let to_serialize = make_test_request();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_REQUEST.len() as u64);
        assert_eq!(&TEST_DATA_REQUEST[..], &actual[..]);
    }

    const TEST_DATA_RESPONSE: [u8; 2] = [
        0x15, // communcation channel
        0x21, // state
    ];

    fn make_test_response() -> ConnectionStateResponse {
        ConnectionStateResponse::new(21, ConnectionState::InvalidConnectionId)
    }

    #[test]
    fn parse_connection_state_response() {
        let (rem, actual) = ConnectionStateResponse::parse(&TEST_DATA_RESPONSE).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_response();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_connection_state_response() {
        let to_serialize = make_test_response();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_RESPONSE.len() as u64);
        assert_eq!(&TEST_DATA_RESPONSE[..], &actual[..]);
    }
}
