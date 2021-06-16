use rusticata_macros::newtype_enum;
use nom_derive::NomBE;
use std::net::SocketAddrV4;

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct HostProtocolCode(u8);

newtype_enum! {
    impl debug HostProtocolCode {
    Ipv4Udp = 0x01,
    Ipv4Tcp = 0x02,
    }
}

impl From<HostProtocolCode> for u8 {
    fn from(f: HostProtocolCode) -> Self {
        f.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct HostProtocolAddressInformation {
    pub protocol_code: HostProtocolCode,
    pub address: SocketAddrV4,
}

impl HostProtocolAddressInformation {
    pub fn new(protocol_code: HostProtocolCode, address: SocketAddrV4) -> Self {
        Self {
            protocol_code,
            address,
        }
    }
}