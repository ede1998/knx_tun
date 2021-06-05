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

#[repr(u8)]
pub enum ProtocolVersion {
    V1_0 = 0x10,
}

pub struct Header {
    header_length: u8,
    protocol_version: ProtocolVersion,
    service_type: ServiceType,
    total_length: u16,
}

impl Header {

    pub fn new(service_type: ServiceType, total_length: u16) -> Self {
        Self {
            header_length: Self::length(),
            protocol_version: ProtocolVersion::V1_0,
            service_type,
            total_length,
        }
    }

    pub const fn length() -> u8 {
        std::mem::size_of::<Self>() as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_header_length() {
        assert_eq!(Header::length(), 6, "Header length must be 6.");
    }
}
