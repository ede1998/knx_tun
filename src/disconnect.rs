use nom_derive::NomBE;

use crate::{core::Body, hpai::Hpai, snack::*};

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct DisconnectRequest {
    communication_channel_id: u8,
    control_endpoint: Hpai,
}

impl From<DisconnectRequest> for Body {
    fn from(f: DisconnectRequest) -> Self {
        Body::DisconnectRequest(f)
    }
}

impl DisconnectRequest {
    pub const fn new(communication_channel_id: u8, control_endpoint: Hpai) -> Self {
        Self {
            communication_channel_id,
            control_endpoint,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            "DisconnectRequest",
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
#[repr(u8)]
#[nom(GenericErrors)]
pub enum DisconnectState {
    /// Disconnect successful
    NoError = 0x00,
    // TODO find other valid status codes.
}

impl From<DisconnectState> for u8 {
    fn from(f: DisconnectState) -> Self {
        f as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct DisconnectResponse {
    communication_channel_id: u8,
    state: DisconnectState,
}

impl From<DisconnectResponse> for Body {
    fn from(f: DisconnectResponse) -> Self {
        Body::DisconnectResponse(f)
    }
}

impl DisconnectResponse {
    pub const fn new(communication_channel_id: u8, state: DisconnectState) -> Self {
        Self {
            communication_channel_id,
            state,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(DisconnectResponse),
            map(tuple((be_u8, DisconnectState::parse)), |(chan, state)| {
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

    const TEST_DATA_DREQUEST: [u8; 10] = [
        0x15, // communication channel
        0x00, // reserved
        0x08, 0x01, 192, 168, 200, 12, 0xC3, 0xB4, // hpai
    ];

    fn make_test_drequest() -> DisconnectRequest {
        DisconnectRequest::new(
            21,
            Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 12], 50100),
        )
    }

    #[test]
    fn parse_disconnect_request() {
        let (rem, actual) = DisconnectRequest::parse(&TEST_DATA_DREQUEST).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_drequest();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_disconnect_request() {
        let to_serialize = make_test_drequest();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_DREQUEST.len() as u64);
        assert_eq!(&TEST_DATA_DREQUEST[..], &actual[..]);
    }

    const TEST_DATA_DRESPONSE: [u8; 2] = [
        0x15, // communcation channel
        0x00, // state
    ];

    fn make_test_dresponse() -> DisconnectResponse {
        DisconnectResponse::new(21, DisconnectState::NoError)
    }

    #[test]
    fn parse_disconnect_response() {
        let (rem, actual) = DisconnectResponse::parse(&TEST_DATA_DRESPONSE).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_dresponse();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_disconnect_response() {
        let to_serialize = make_test_dresponse();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_DRESPONSE.len() as u64);
        assert_eq!(&TEST_DATA_DRESPONSE[..], &actual[..]);
    }
}
