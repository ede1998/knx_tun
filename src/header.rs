use nom_derive::NomBE;
use rusticata_macros::newtype_enum;

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ServiceType(u16);

newtype_enum! {
    impl debug ServiceType {
    SearchRequest = 0x0201,
    SearchResponse = 0x0202,
    DescriptionRequest = 0x0203,
    DescriptionResponse = 0x0204,
    ConnectRequest = 0x0205,
    ConnectResponse = 0x0206,
    ConnectionstateRequest = 0x0207,
    ConnectionstateResponse = 0x0208,
    DisconnectRequest = 0x0209,
    DisconnectResponse = 0x020A,
    TunnelRequest = 0x0420,
    TunnelResponse = 0x0421,
    DeviceConfigurationRequest = 0x0310,
    DeviceConfigurationAck = 0x0311,
    RoutingIndication = 0x0530,
    RoutingLostMessage = 0x531,
}
}

impl From<ServiceType> for u16 {
    fn from(s: ServiceType) -> Self {
        s.0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ProtocolVersion(u8);

newtype_enum! {
    impl debug ProtocolVersion {
    V1_0 = 0x10,
}
}

impl From<ProtocolVersion> for u8 {
    fn from(pv: ProtocolVersion) -> Self {
        pv.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Header {
    pub version: ProtocolVersion,
    pub service_type: ServiceType,
    pub body_length: u16,
}

impl Header {
    pub const fn new(service_type: ServiceType, body_length: u16) -> Self {
        Self {
            version: ProtocolVersion::V1_0,
            service_type,
            body_length,
        }
    }

    pub const LENGTH: u16 = 0x06;
}