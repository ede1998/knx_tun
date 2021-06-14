use nom::number::streaming::be_u8;
use nom::{Err, IResult};
use nom::error::{make_error, ErrorKind};
use nom_derive::*;
use rusticata_macros::newtype_enum;

use crate::hpai::HostProtocolAddressInformation;

/// Establish a Data Link Layer tunnel to the KNX network.
/// Establish a raw tunnel to the KNX network.
/// Establish a Busmonitor tunnel to the KNX network.
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct KnxLayer(u8);

newtype_enum! {
impl debug KnxLayer {
    LinkLayer = 0x02,
    Raw = 0x04,
    BusMonitor = 0x80,
}
}

impl From<KnxLayer> for u8 {
    fn from(v: KnxLayer) -> Self {
        v.0
    }
}

/// Data connection used for configuration and data transfer with a remote logging server.
/// Data connection used for configuration and data transfer with an Object Server in a KNXnet/IP device.
/// Data connection used for data transfer with a remote configuration server.
/// Data connection used to configure a KNXnet/IP device
/// Data connection used to forward KNX telegrams between two KNXnet/IP devices.
#[derive(Clone, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ConnectionType(u8);

newtype_enum! {
impl debug ConnectionType {
    DeviceManagement = 0x03,
    Tunnel = 0x04,
    RemoteLogging = 0x06,
    RemoteConfiguration = 0x07,
    ObjectServer = 0x08,
}
}

impl From<ConnectionType> for u8 {
    fn from(v: ConnectionType) -> Self {
        v.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ConnectionInformationHeader {
    length: u8,
    pub connection_type: ConnectionType,
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum ConnectionRequestInformation {
    DeviceManagement,
    ObjectServer,
    RemoteConfiguration,
    RemoteLogging,
    Tunnel(KnxLayer),
}

impl ConnectionRequestInformation {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            Self::Tunnel(layer) => vec![0x04, ConnectionType::Tunnel.into(), (*layer).into(), 0x00],
            _ => todo!(),
        }
    }

    pub const fn len() -> u8 {
        4
    }
}

pub(crate) fn parse_connection_request_information(i: &[u8]) -> IResult<&[u8], ConnectionRequestInformation> {
    let (i, header) = ConnectionInformationHeader::parse(i)?;
    match header.connection_type {
        ConnectionType::Tunnel => parse_connection_request_information_tunnel(i),
        _ => Err(Err::Error(make_error(i, ErrorKind::Switch))),
    }
}

pub(crate) fn parse_connection_request_information_tunnel(i: &[u8]) -> IResult<&[u8], ConnectionRequestInformation> {
    let (i, layer) = KnxLayer::parse(i)?;
    let (i, _) = be_u8(i)?;
    Ok((i, ConnectionRequestInformation::Tunnel(layer)))
}

pub(crate) fn parse_connection_response_data_block(i: &[u8]) -> IResult<&[u8], ConnectionResponseDataBlock> {
    let (i, header) = ConnectionInformationHeader::parse(i)?;
    todo!();
}
pub enum ConnectionResponseDataBlock {
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectRequest {
    pub control_endpoint: HostProtocolAddressInformation,
    pub data_endpoint: HostProtocolAddressInformation,
    pub cri: ConnectionRequestInformation,
}

impl ConnectRequest {
    pub fn serialize(&self) -> Vec<u8> {
        let mut result = self.control_endpoint.serialize();
        result.extend_from_slice(&self.data_endpoint.serialize());
        result.extend_from_slice(&self.cri.serialize());
        result
    }

    pub const fn len() -> u16 {
        HostProtocolAddressInformation::len() as u16 * 2 + ConnectionRequestInformation::len() as u16
    }
}