use mac_address::MacAddress;
use nom::bytes::streaming::take;
use nom::error::{make_error, ErrorKind};
use nom::multi::count;
use nom::number::streaming::be_u8;
use nom::{Err, IResult};
use nom_derive::*;
use rusticata_macros::newtype_enum;
use std::convert::TryInto;
use std::net::Ipv4Addr;

use crate::address::parse_address;
use crate::address::Address;

#[derive(Clone, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct DescriptionType(u8);

newtype_enum! {
impl debug DescriptionType {
    DeviceInfo = 0x01,
    SupportedServiceFamilies = 0x02,
    IpConfiguration = 0x03,
    IpCurrentConfiguration = 0x04,
    KnxAddresses = 0x05,
    ManufacturerData = 0xFE,
}
}

impl From<DescriptionType> for u8 {
    fn from(v: DescriptionType) -> Self {
        v.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct DescriptionInformationBlockHeader {
    length: u8,
    pub connection_type: DescriptionType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DescriptionInformationBlock {
    DeviceInfo(DeviceInfo),
    SupportedServiceFamilies(Vec<ServiceFamily>),
    IpConfiguration,
    IpCurrentConfiguration,
    KnxAddresses,
    ManufacturerData,
}
fn map<T,U,F>(input: (&[u8], T), f: F) -> IResult<&[u8], U>
where F: FnOnce(T) -> U
{
    Ok((input.0, f(input.1)))
}

pub(crate) fn parse_description_information_block(i: &[u8]) -> IResult<&[u8], DescriptionInformationBlock> {
    let (i, header) = DescriptionInformationBlockHeader::parse(i)?;
    use DescriptionInformationBlock::*;
    match header.connection_type {
        DescriptionType::DeviceInfo => map(parse_device_information(i)?, |info| DeviceInfo(info)),
        DescriptionType::SupportedServiceFamilies => map(parse_service_families(i, header.length)?, |fam| SupportedServiceFamilies(fam)),
        _ => Err(Err::Error(make_error(i, ErrorKind::Switch))),
    }
}

/// Chapter 7.5.4.2
#[derive(Clone, Debug, PartialEq)]
pub struct DeviceInfo {
    pub knx_medium: KnxMedium,
    pub device_status: DeviceStatus,
    pub knx_individual_address: Address,
    pub project_installation_identifier: ProjectInstallationIdentifier,
    pub serial_number: SerialNumber,
    pub routing_multicast_address: Ipv4Addr,
    pub mac_address: MacAddress,
    pub friendly_name: FriendlyName,
}

pub(crate) fn parse_device_information(i: &[u8]) -> IResult<&[u8], DeviceInfo> {
    let (i, knx_medium) = KnxMedium::parse(i)?;
    let (i, device_status) = parse_device_status(i)?;
    let (i, address) = parse_address(i, crate::address::AddressKind::Individual)?;
    let (i, ident) = ProjectInstallationIdentifier::parse(i)?;
    let (i, serial_number) = parse_serial_number(i)?;
    let (i, multicast_address) = parse_ipv4_address(i)?;
    if !multicast_address.is_multicast() {
        return Err(Err::Error(make_error(i, ErrorKind::Verify)));
    }
    let (i, mac_address) = parse_mac_address(i)?;
    let (i, friendly_name) = parse_friendly_name(i)?;
    let info = DeviceInfo {
        knx_medium,
        device_status,
        knx_individual_address: address,
        project_installation_identifier: ident,
        serial_number,
        routing_multicast_address: multicast_address,
        mac_address,
        friendly_name,
    };
    Ok((i, info))
}

pub(crate) fn parse_ipv4_address(i: &[u8]) -> IResult<&[u8], Ipv4Addr> {
    let (i, octets) = take(4usize)(i)?;
    let octets: [u8; 4] = octets
        .try_into()
        .map_err(|_| Err::Error(make_error(i, ErrorKind::Verify)))?;
    Ok((i, octets.into()))
}

pub(crate) fn parse_mac_address(i: &[u8]) -> IResult<&[u8], MacAddress> {
    let (i, bytes) = take(6usize)(i)?;
    let bytes: [u8; 6] = bytes
        .try_into()
        .map_err(|_| Err::Error(make_error(i, ErrorKind::Verify)))?;
    Ok((i, bytes.into()))
}

#[derive(Clone, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct KnxMedium(u8);

newtype_enum! {
impl debug KnxMedium {
    Tp1 = 0x02,
    Pl110 = 0x04,
    Rf = 0x10,
    KnxIp = 0x20,
}
}

impl From<KnxMedium> for u8 {
    fn from(v: KnxMedium) -> Self {
        v.0
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct DeviceStatus {
    pub programming_mode: bool,
}

pub(crate) fn parse_device_status(i: &[u8]) -> IResult<&[u8], DeviceStatus> {
    let (i, value) = be_u8(i)?;
    let programming_mode = (value & 0b0000_0001) != 0;
    Ok((i, DeviceStatus { programming_mode }))
}

impl From<DeviceStatus> for u8 {
    fn from(d: DeviceStatus) -> Self {
        let programming_mode = u8::from(d.programming_mode).to_be();
        programming_mode
    }
}

/// Bits 15 to 4 project number
/// Bits 3 to 0 installation number
#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ProjectInstallationIdentifier(u16);

impl ProjectInstallationIdentifier {
    pub fn project_number(&self) -> u16 {
        (self.0 & 0xFFF0) >> 4
    }

    pub fn installation_number(&self) -> u16 {
        self.0 & 0xF
    }
}

impl From<ProjectInstallationIdentifier> for u16 {
    fn from(i: ProjectInstallationIdentifier) -> Self {
        i.0.to_be()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct SerialNumber([u8; 6]);

pub(crate) fn parse_serial_number(i: &[u8]) -> IResult<&[u8], SerialNumber> {
    let (i, number) = take(6usize)(i)?;
    let number = number
        .try_into()
        .map_err(|_| Err::Error(make_error(i, ErrorKind::Verify)))?;
    Ok((i, SerialNumber(number)))
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct FriendlyName([u8; 30]);

impl FriendlyName {
    fn valid_iso8859_1(s: &str) -> bool {
        s.chars().all(|c| c as u32 <= 0xFF)
    }

    pub fn new(s: String) -> Result<Self, FriendlyNameCreationError> {
        match s {
            s if Self::valid_iso8859_1(&s) => Err(FriendlyNameCreationError::InvalidCharacters),
            s if s.len() > 29 => Err(FriendlyNameCreationError::TooLong),
            s => {
                let mut name = [0u8; 30];
                let mut index = 0;
                for c in name.iter_mut() {
                    *c = s.as_bytes()[index];
                    index += 1;
                }
                Ok(FriendlyName(name))
            }
        }
    }

    pub fn from_iso8859_1_slice(s: &[u8]) -> Result<Self, FriendlyNameCreationError> {
        match s {
            s if s.len() > 30 => Err(FriendlyNameCreationError::TooLong),
            s => {
                let mut name = [0u8; 30];
                let mut index = 0;
                for c in name.iter_mut() {
                    *c = s[index];
                    index += 1;
                }
                Ok(FriendlyName(name))
            }
        }
    }
}

impl From<FriendlyName> for String {
    fn from(n: FriendlyName) -> Self {
        n.0.iter().map(|&c| c as char).collect()
    }
}

pub enum FriendlyNameCreationError {
    TooLong,
    InvalidCharacters,
}

pub(crate) fn parse_friendly_name(i: &[u8]) -> IResult<&[u8], FriendlyName> {
    let (i, r) = take(30usize)(i)?;
    let name = FriendlyName::from_iso8859_1_slice(r)
        .map_err(|_| Err::Error(make_error(i, ErrorKind::Verify)))?;
    Ok((i, name))
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ServiceFamily {
    pub family_id: ServiceFamilyId,
    pub version: u8,
}

pub(crate) fn parse_service_families(i: &[u8], length: u8) -> IResult<&[u8], Vec<ServiceFamily>> {
    let header_size = 2; // std::mem::size_of::<DescriptionInformationBlockHeader>();
    let service_family_size = 2; // std::mem::size_of::<ServiceFamily>();
    let num = (length as usize - header_size) / service_family_size;
    count(ServiceFamily::parse, num)(i)
}

#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ServiceFamilyId(u8);

newtype_enum! {
impl debug ServiceFamilyId {
    Core = 0x02,
    DeviceManagement = 0x03,
    Tunnelling = 0x04,
    Routing = 0x05,
    RemoteLogging = 0x06,
    RemoteConfiguration = 0x07,
    ObjectServer = 0x08,
}
}

impl From<ServiceFamilyId> for u8 {
    fn from(id: ServiceFamilyId) -> Self {
        id.0.to_be()
    }
}
