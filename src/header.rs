
#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
#[repr(u16)]
pub enum ServiceType {
    SearchRequest = 0x0201,
    SearchResponse = 0x0202,
    DescriptionRequest = 0x0203,
    DescriptionResponse = 0x0204,
    ConnectionRequest = 0x0205,
    ConnectionResponse = 0x0206,
    ConnectionstateRequest = 0x0207,
    ConnectionstateResponse = 0x0208,
    DisconnectRequest = 0x0209,
    DisconnectResponse = 0x020A,
    TunnelRequest = 0x0420,
    TunnelResponse = 0x0421,
    DeviceConfigurationRequest = 0x0310,
    DeviceConfigurationAck = 0x0311,
    RoutingIndication = 0x0530,
}

impl ServiceType {
    pub fn serialize(&self) -> [u8; 2] {
        (*self as u16).to_be_bytes()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
#[repr(u8)]
pub enum ProtocolVersion {
    V1_0 = 0x10,
}

impl ProtocolVersion {
    pub fn serialize(&self) -> u8 {
        (*self as u8).to_be()
    }
}

pub struct Header {
    header_length: u8,
    version: ProtocolVersion,
    service_type: ServiceType,
    total_length: u16,
}

impl Header {
    pub const fn new(service_type: ServiceType, total_length: u16) -> Self {
        Self {
            header_length: Self::length() as u8,
            version: ProtocolVersion::V1_0,
            service_type,
            total_length,
        }
    }

    pub const fn length() -> usize {
        std::mem::size_of::<Self>()
    }

    pub fn serialize(&self) -> Vec<u8> {
        let mut result = vec![self.header_length.to_be(), self.version.serialize()];
        result.extend_from_slice(&self.service_type.serialize());
        result.extend_from_slice(&self.total_length.to_be_bytes());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_header_length() {
        assert_eq!(Header::length(), 6, "Header length must be 6.");
    }

    #[test]
    fn serialize_header() {
        let header = Header::new(ServiceType::DescriptionRequest, 0x1234);
        let serialized = header.serialize();
        
        println!("{:#x?}", serialized);
        assert_eq!(serialized.len(), Header::length(), "Header has wrong length.");
        assert_eq!(serialized[0], 6, "Wrong header length in serialization.");
        assert_eq!(serialized[1], 0x10, "Wrong protocol version in serialization.");
        assert_eq!(serialized[2], 0x02, "Wrong service_type high value in serialization.");
        assert_eq!(serialized[3], 0x03, "Wrong service_type low value in serialization.");
        assert_eq!(serialized[4], 0x12, "Wrong total length high value in serialization.");
        assert_eq!(serialized[5], 0x34, "Wrong total length high value in serialization.");

    }
}
