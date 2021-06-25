use crate::address::Address;
use crate::make_tag;
use crate::snack::*;
use nom_derive::*;

use crate::core::Body;
use crate::hpai::Hpai;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum KnxLayer {
    /// Establish a Data Link Layer tunnel to the KNX network.
    LinkLayer = 0x02,
    /// Establish a raw tunnel to the KNX network.
    Raw = 0x04,
    /// Establish a Busmonitor tunnel to the KNX network.
    BusMonitor = 0x80,
}

impl From<KnxLayer> for u8 {
    fn from(v: KnxLayer) -> Self {
        v as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum Cri {
    /// Data connection used to configure a KNXnet/IP device
    DeviceManagement,
    /// Data connection used for configuration and data transfer with an Object Server in a KNXnet/IP device.
    ObjectServer,
    /// Data connection used for data transfer with a remote configuration server.
    RemoteConfiguration,
    /// Data connection used for configuration and data transfer with a remote logging server.
    RemoteLogging,
    /// Data connection used to forward KNX telegrams between two KNXnet/IP devices.
    Tunnel(TunnelRequest),
}

impl From<TunnelRequest> for Cri {
    fn from(f: TunnelRequest) -> Self {
        Self::Tunnel(f)
    }
}

impl Cri {
    pub const fn new_tunnel(layer: KnxLayer) -> Self {
        Self::Tunnel(TunnelRequest { layer })
    }

    pub(crate) fn parse(i: &[u8]) -> IResult<Self> {
        use nm::*;
        context("ConnectRequestInformation", into(TunnelRequest::parse))(i)
        // alt((
        //     into(Tunnel::parse),
        //     into(...)
        // ))(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        move |out| match self {
            Self::Tunnel(t) => t.gen()(out),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct TunnelRequest {
    pub layer: KnxLayer,
}

impl TunnelRequest {
    make_tag! {0x04, u8}
    pub(crate) fn parse(i: &[u8]) -> IResult<Self> {
        use nm::*;
        context(
            "CRI-Tunnel",
            length_value_incl(
                be_u8,
                map(
                    tuple((
                        tag(Self::TAG),
                        KnxLayer::parse,
                        be_u8, // reserved byte
                    )),
                    |(_, l, _)| Self { layer: l },
                ),
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        length_data_incl(
            1,
            tuple((
                slice(Self::TAG),
                be_u8(self.layer.into()),
                be_u8(0), // reserved byte
            )),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectRequest {
    pub control_endpoint: Hpai,
    pub data_endpoint: Hpai,
    pub cri: Cri,
}

impl From<ConnectRequest> for Body {
    fn from(f: ConnectRequest) -> Self {
        Self::ConnectRequest(f)
    }
}

impl ConnectRequest {
    pub const fn new(control_endpoint: Hpai, data_endpoint: Hpai, cri: Cri) -> Self {
        Self {
            control_endpoint,
            data_endpoint,
            cri,
        }
    }

    pub(crate) fn parse(i: &[u8]) -> IResult<Self> {
        use nm::*;
        context(
            "ConnectRequest",
            map(
                tuple((Hpai::parse, Hpai::parse, Cri::parse)),
                |(ctl, data, cri)| ConnectRequest {
                    control_endpoint: ctl,
                    data_endpoint: data,
                    cri,
                },
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((
            self.control_endpoint.gen(),
            self.data_endpoint.gen(),
            self.cri.gen(),
        ))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum ConnectionTypeCode {
    /// Data connection used to configure a KNXnet/IP device
    DeviceManagement = 0x03,
    /// Data connection used to forward KNX telegrams between two KNXnet/IP devices.
    Tunnel = 0x04,
    /// Data connection used for configuration and data transfer with a remote logging server.
    RemoteLogging = 0x06,
    /// Data connection used for data transfer with a remote configuration server.
    RemoteConfiguration = 0x07,
    /// Data connection used for configuration and data transfer with an Object Server in a KNXnet/IP device.
    ObjectServer = 0x08,
}

impl From<ConnectionTypeCode> for u8 {
    fn from(f: ConnectionTypeCode) -> Self {
        f as u8
    }
}

impl ConnectionTypeCode {
    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        cf::be_u8((*self).into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectResponse {
    pub communication_channel_id: u8,
    pub state: ConnectResponseState,
    pub data_endpoint: Hpai,
    pub crd: Crd,
}

impl ConnectResponse {
    pub fn new(
        communication_channel_id: u8,
        state: ConnectResponseState,
        data_endpoint: Hpai,
        crd: Crd,
    ) -> Self {
        Self {
            communication_channel_id,
            state,
            data_endpoint,
            crd,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context("ConnectResponse", |i| {
            let (i, channel_id) = be_u8(i)?;
            let (i, state) = ConnectResponseState::parse(i)?;
            let (i, hpai) = Hpai::parse(i)?;
            let (i, crd) = Crd::parse(i)?;
            Ok((i, Self::new(channel_id, state, hpai, crd)))
        })(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((
            be_u8(self.communication_channel_id),
            be_u8(self.state.into()),
            self.data_endpoint.gen(),
            self.crd.gen(),
        ))
    }
}

impl From<ConnectResponse> for Body {
    fn from(f: ConnectResponse) -> Self {
        Self::ConnectResponse(f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum ConnectResponseState {
    ///  The connection is established successfully.
    NoError = 0x00,
    /// The requested connection type is not supported by the KNXnet/IP Server device.
    UnsupportedConnectionType = 0x22,
    /// One or more requested connection options are not supported by the KNXnet/IP Server device.
    UnsupportedConnectionOption = 0x23,
    /// The KNXnet/IP Server device cannot accept the new data connection because its maximum amount of concurrent connections is already occupied.
    NoMoreConnections = 0x24,
}

impl From<ConnectResponseState> for u8 {
    fn from(f: ConnectResponseState) -> Self {
        f as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum Crd {
    /// Data connection used to configure a KNXnet/IP device
    DeviceManagement,
    /// Data connection used for configuration and data transfer with an Object Server in a KNXnet/IP device.
    ObjectServer,
    /// Data connection used for data transfer with a remote configuration server.
    RemoteConfiguration,
    /// Data connection used for configuration and data transfer with a remote logging server.
    RemoteLogging,
    /// Data connection used to forward KNX telegrams between two KNXnet/IP devices.
    Tunnel(TunnelResponse),
}

impl From<TunnelResponse> for Crd {
    fn from(f: TunnelResponse) -> Self {
        Self::Tunnel(f)
    }
}

impl Crd {
    pub fn new_tunnel(addr: Address) -> Self {
        TunnelResponse::new(addr).into()
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            "Crd",
            length_value_incl(be_u8, |i| {
                let (i, connection_type) = ConnectionTypeCode::parse(i)?;
                match connection_type {
                    ConnectionTypeCode::Tunnel => into(TunnelResponse::parse)(i),
                    _ => unimplemented!(),
                }
            }),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        use ConnectionTypeCode::*;
        length_data_incl(1, move |out| match self {
            Self::Tunnel(t) => tuple((Tunnel.gen(), t.gen()))(out),
            _ => unimplemented!(),
        })
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct TunnelResponse {
    knx_individual_address: Address,
}

impl TunnelResponse {
    pub fn new(addr: Address) -> Self {
        Self {
            knx_individual_address: addr,
        }
    }
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context("CRD-Tunnel", |i| {
            let (i, addr) = Address::parse(i, crate::address::AddressKind::Individual)?;
            Ok((i, Self::new(addr)))
        })(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        self.knx_individual_address.gen()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hpai::HostProtocolCode;

    const TEST_DATA_CREQUEST: [u8; 20] = [
        0x08, 0x01, 192, 168, 200, 12, 0xC3, 0xB4, // ctrl hpai
        0x08, 0x01, 192, 168, 200, 20, 0xC3, 0xB5, // data hpai
        0x04, 0x04, 0x02, 0x00, // cri
    ];

    fn make_test_crequest() -> ConnectRequest {
        ConnectRequest::new(
            Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 12], 50100),
            Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 20], 50101),
            Cri::new_tunnel(KnxLayer::LinkLayer),
        )
    }

    #[test]
    fn parse_connect_request() {
        let (rem, actual) = ConnectRequest::parse(&TEST_DATA_CREQUEST).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_crequest();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_connect_request() {
        let to_serialize = make_test_crequest();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_CREQUEST.len() as u64);
        assert_eq!(&TEST_DATA_CREQUEST[..], &actual[..]);
    }

    const TEST_DATA_CRESPONSE: [u8; 14] = [
        0x15, // communication channel
        0x00, // status code
        0x08, 0x01, 192, 168, 200, 20, 0xC3, 0xB4, // hpai
        0x04, 0x04, 0x11, 0x0A, // crd
    ];

    fn make_test_cresponse() -> ConnectResponse {
        ConnectResponse::new(
            0x15,
            ConnectResponseState::NoError,
            Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 20], 50100),
            Crd::new_tunnel(Address::new(
                crate::address::AddressKind::Individual,
                0x11,
                0xA,
            )),
        )
    }

    #[test]
    fn parse_connect_response() {
        let (rem, actual) = ConnectResponse::parse(&TEST_DATA_CRESPONSE).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_cresponse();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_connect_response() {
        let to_serialize = make_test_cresponse();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_CRESPONSE.len() as u64);
        assert_eq!(&TEST_DATA_CRESPONSE[..], &actual[..]);
    }
}
