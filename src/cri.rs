use crate::make_tag;
use crate::snack::*;
use nom_derive::*;

use crate::core::Body;
use crate::hpai::HostProtocolAddressInformation;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
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
pub enum ConnectionRequestInformation {
    /// 0x03 Data connection used to configure a KNXnet/IP device
    DeviceManagement,
    /// 0x08 Data connection used for configuration and data transfer with an Object Server in a KNXnet/IP device.
    ObjectServer,
    /// 0x07 Data connection used for data transfer with a remote configuration server.
    RemoteConfiguration,
    /// 0x06 Data connection used for configuration and data transfer with a remote logging server.
    RemoteLogging,
    /// 0x04 Data connection used to forward KNX telegrams between two KNXnet/IP devices.
    Tunnel(Tunnel),
}

impl From<Tunnel> for ConnectionRequestInformation {
    fn from(f: Tunnel) -> Self {
        Self::Tunnel(f)
    }
}

impl ConnectionRequestInformation {
    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        use nm::*;
        into(Tunnel::parse)(i)
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
pub struct Tunnel {
    pub layer: KnxLayer,
}

impl Tunnel {
    make_tag! {0x04, u8}
    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        use nm::*;
        length_value_incl(
            be_u8,
            map(tuple((tag(Self::TAG), KnxLayer::parse)), |(_, l)| Self {
                layer: l,
            }),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        length_data_incl(1, tuple((slice(Self::TAG), be_u8(self.layer.into()))))
    }
}

pub enum ConnectionResponseDataBlock {}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectRequest {
    pub control_endpoint: HostProtocolAddressInformation,
    pub data_endpoint: HostProtocolAddressInformation,
    pub cri: ConnectionRequestInformation,
}

impl From<ConnectRequest> for Body {
    fn from(f: ConnectRequest) -> Self {
        Self::ConnectRequest(f)
    }
}

impl ConnectRequest {
    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        use nm::*;
        type Hpai = HostProtocolAddressInformation;
        type Cri = ConnectionRequestInformation;
        map(
            tuple((Hpai::parse, Hpai::parse, Cri::parse)),
            |(ctl, data, cri)| ConnectRequest {
                control_endpoint: ctl,
                data_endpoint: data,
                cri,
            },
        )(i)
    }
}
