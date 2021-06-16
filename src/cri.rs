use nom::number::streaming::be_u8;
use nom::{Err, IResult};
use nom::error::{make_error, ErrorKind};
use nom_derive::*;
use rusticata_macros::newtype_enum;

use crate::hpai::HostProtocolAddressInformation;

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct KnxLayer(u8);

newtype_enum! {
impl debug KnxLayer {
    LinkLayer = 0x02, // Establish a Data Link Layer tunnel to the KNX network.
    Raw = 0x04, // Establish a raw tunnel to the KNX network.
    BusMonitor = 0x80, // Establish a Busmonitor tunnel to the KNX network.
}
}

impl From<KnxLayer> for u8 {
    fn from(v: KnxLayer) -> Self {
        v.0
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ConnectionType(u8);

newtype_enum! {
impl debug ConnectionType {
    DeviceManagement = 0x03, // Data connection used to configure a KNXnet/IP device
    Tunnel = 0x04, // Data connection used to forward KNX telegrams between two KNXnet/IP devices.
    RemoteLogging = 0x06, // Data connection used for configuration and data transfer with a remote logging server.
    RemoteConfiguration = 0x07, // Data connection used for data transfer with a remote configuration server.
    ObjectServer = 0x08, // Data connection used for configuration and data transfer with an Object Server in a KNXnet/IP device.
}
}

impl From<ConnectionType> for u8 {
    fn from(v: ConnectionType) -> Self {
        v.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum ConnectionRequestInformation {
    DeviceManagement,
    ObjectServer,
    RemoteConfiguration,
    RemoteLogging,
    Tunnel(Tunnel),
}

impl ConnectionRequestInformation {
    pub fn as_connection_type(&self) -> ConnectionType {
        match self {
            Self::DeviceManagement => ConnectionType::DeviceManagement,
            Self::ObjectServer => ConnectionType::ObjectServer,
            Self::RemoteConfiguration => ConnectionType::RemoteConfiguration,
            Self::RemoteLogging => ConnectionType::RemoteLogging,
            Self::Tunnel(_) => ConnectionType::Tunnel,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Tunnel {
    pub layer: KnxLayer,
}

pub enum ConnectionResponseDataBlock {
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectRequest {
    pub control_endpoint: HostProtocolAddressInformation,
    pub data_endpoint: HostProtocolAddressInformation,
    pub cri: ConnectionRequestInformation,
}