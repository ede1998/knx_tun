use std::net::SocketAddrV4;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum HostProtocolCode {
    Ipv4Udp = 0x01,
    Ipv4Tcp = 0x02,
}

impl HostProtocolCode {
    pub const fn serialize(&self) -> u8 {
        (*self as u8).to_be()
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

    pub const fn len() -> u8 {
        0x08
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(Self::len() as usize);
        result.push(Self::len());
        result.push(self.protocol_code.serialize());
        let ip = self.address.ip().octets();
        result.extend(ip.iter().map(|x| x.to_be()));
        let port = self.address.port();
        result.extend_from_slice(&port.to_be_bytes());
        result
    }
}

#[cfg(test)]
mod tests {
    use std::net::Ipv4Addr;

    use super::*;

    #[test]
    fn correct_host_protocol_address_information_length() {
        let len = HostProtocolAddressInformation::len();
        assert_eq!(len, 8, "Length must be 8.");
    }

    #[test]
    fn serialize_host_protocol_address_information() {
        let info = HostProtocolAddressInformation::new(
            HostProtocolCode::Ipv4Udp,
            SocketAddrV4::new(Ipv4Addr::new(127, 143, 231, 144), 48),
        );
        let serialized = info.serialize();

        println!("{:?}", serialized);
        assert_eq!(serialized.len(), 008, "Wrong length.");
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
