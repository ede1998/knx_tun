use crate::make_tag;
use crate::snack::*;
use nom_derive::*;

use crate::core::Body;
use crate::hpai::Hpai;

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
pub enum Cri {
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

impl From<Tunnel> for Cri {
    fn from(f: Tunnel) -> Self {
        Self::Tunnel(f)
    }
}

impl Cri {
    pub fn new_tunnel(layer: KnxLayer) -> Self {
        Tunnel { layer }.into()
    }

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
    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        use nm::*;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hpai::HostProtocolCode;

    #[test]
    fn parse_connect_request() {
        #[rustfmt::skip]
        let serialized = [
            0x08, 0x01, 192, 168, 200, 12, 0xC3, 0xB4, // ctrl hpai
            0x08, 0x01, 192, 168, 200, 20, 0xC3, 0xB5, // data hpai
            0x04, 0x04, 0x02, 0x00, // cri
        ];

        let (rem, cr) = ConnectRequest::parse(&serialized).unwrap();

        let ctrl = Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 12], 50100);
        let data = Hpai::new_from_parts(HostProtocolCode::Ipv4Udp, [192, 168, 200, 20], 50101);
        assert_eq!(0, rem.len());
        assert_eq!(cr.data_endpoint, data);
        assert_eq!(cr.control_endpoint, ctrl);
        assert_eq!(cr.cri, Cri::new_tunnel(KnxLayer::LinkLayer));
    }
}
