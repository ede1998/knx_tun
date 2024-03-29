use crate::snack::*;
use nom_derive::NomBE;
use std::net::{Ipv4Addr, SocketAddrV4};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Hpai {
    pub protocol_code: HostProtocolCode,
    /// Only IPv4 supported right now. (Core 8.6.2)
    pub address: SocketAddrV4,
}

impl Hpai {
    pub const fn new(protocol_code: HostProtocolCode, address: SocketAddrV4) -> Self {
        Self {
            protocol_code,
            address,
        }
    }

    pub fn new_from_parts<T: Into<Ipv4Addr>>(
        protocol_code: HostProtocolCode,
        ip: T,
        port: u16,
    ) -> Self {
        Self {
            protocol_code,
            address: SocketAddrV4::new(ip.into(), port),
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

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            "HPAI",
            length_value_incl(
                be_u8,
                map(
                    tuple((HostProtocolCode::parse, be_u32, be_u16)),
                    |(code, ip, port)| Self::new(code, SocketAddrV4::new(ip.into(), port)),
                ),
            ),
        )(i)
    }
}

#[cfg(test)]
mod tests {
    use std::net::{Ipv4Addr, SocketAddrV4};

    use super::*;

    #[test]
    fn gen_host_protocol_address_information() {
        let info = Hpai::new(
            HostProtocolCode::Ipv4Udp,
            SocketAddrV4::new(Ipv4Addr::new(127, 143, 231, 144), 48),
        );

        let (serialized, len) = cookie_factory::gen(info.gen(), vec![]).unwrap();

        println!("{:?}", serialized);
        assert_eq!(serialized.len(), 8, "Wrong length.");
        assert_eq!(len, 8, "Wrong length in result.");
        assert_eq!(serialized[0], 8, "Wrong information length.");
        assert_eq!(serialized[1], 1, "Wrong protocol code.");
        assert_eq!(serialized[2], 127, "Wrong ip address octet 1.");
        assert_eq!(serialized[3], 143, "Wrong ip address octet 2.");
        assert_eq!(serialized[4], 231, "Wrong ip address octet 3.");
        assert_eq!(serialized[5], 144, "Wrong ip address octet 4.");
        assert_eq!(serialized[6], 0, "Wrong port low value.");
        assert_eq!(serialized[7], 48, "Wrong port high value.");
    }

    #[test]
    fn parse_host_protocol_address_information() {
        let serialized = [0x08, 0x01, 192, 168, 200, 12, 0xC3, 0xB4];

        let (rem, hpai) = Hpai::parse(&serialized).unwrap();

        let expected = Hpai::new(
            HostProtocolCode::Ipv4Udp,
            SocketAddrV4::new(Ipv4Addr::new(192, 168, 200, 12), 50100),
        );
        assert_eq!(0, rem.len());
        assert_eq!(hpai, expected);
    }
}
