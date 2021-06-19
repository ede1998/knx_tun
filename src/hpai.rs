use crate::snack::*;
use nom_derive::NomBE;
use std::net::SocketAddrV4;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[repr(u8)]
pub enum HostProtocolCode {
    Ipv4Udp = 0x01,
    Ipv4Tcp = 0x02,
}

impl From<HostProtocolCode> for u8 {
    fn from(f: HostProtocolCode) -> Self {
        f as u8
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

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        length_data_incl(
            1,
            tuple((
                be_u8(self.protocol_code.into()),
                slice(self.address.ip().octets()),
                be_u16(self.address.port()),
            )),
        )
    }

    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        use nm::*;
        let (i, protocol_code) = HostProtocolCode::parse(i)?;
        let (i, ip) = be_u32(i)?;
        let (i, port) = be_u16(i)?;
        let address = SocketAddrV4::new(ip.into(), port);
        Ok((i, Self::new(protocol_code, address)))
    }
}

#[cfg(test)]
mod tests {
    use std::net::{Ipv4Addr, SocketAddrV4};

    use super::*;

    #[test]
    fn gen_host_protocol_address_information() {
        let info = HostProtocolAddressInformation::new(
            HostProtocolCode::Ipv4Udp,
            SocketAddrV4::new(Ipv4Addr::new(127, 143, 231, 144), 48),
        );

        let (serialized, len) = cookie_factory::gen(info.gen(), vec![]).unwrap();

        println!("{:?}", serialized);
        assert_eq!(serialized.len(), 008, "Wrong length.");
        assert_eq!(len, 008, "Wrong length in result.");
        assert_eq!(serialized[0], 008, "Wrong information length.");
        assert_eq!(serialized[1], 001, "Wrong protocol code.");
        assert_eq!(serialized[2], 127, "Wrong ip address octet 1.");
        assert_eq!(serialized[3], 143, "Wrong ip address octet 2.");
        assert_eq!(serialized[4], 231, "Wrong ip address octet 3.");
        assert_eq!(serialized[5], 144, "Wrong ip address octet 4.");
        assert_eq!(serialized[6], 000, "Wrong port low value.");
        assert_eq!(serialized[7], 048, "Wrong port high value.");
    }
}
