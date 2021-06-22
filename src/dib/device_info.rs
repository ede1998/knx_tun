use mac_address::MacAddress;
use nom_derive::*;
use std::net::Ipv4Addr;

use crate::address::{Address, AddressKind};
use crate::snack::*;

/// Chapter 7.5.4.2
#[derive(Clone, Debug, PartialEq)]
pub struct DeviceInfo {
    pub knx_medium: KnxMedium,
    pub device_status: DeviceStatus,
    pub knx_individual_address: Address,
    pub project_installation_identifier: ProjectInstallationIdentifier,
    pub serial_number: [u8; 6],
    pub routing_multicast_address: Ipv4Addr,
    pub mac_address: MacAddress,
    pub friendly_name: StringBuffer<30>,
}

impl DeviceInfo {
    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], DeviceInfo> {
        use nm::*;
        let (i, knx_medium) = KnxMedium::parse(i)?;
        let (i, device_status) = DeviceStatus::parse(i)?;
        let (i, address) = Address::parse(i, AddressKind::Individual)?;
        let (i, ident) = ProjectInstallationIdentifier::parse(i)?;
        let (i, serial_number) = fixed_slice::<6>(i)?;
        let (i, multicast_address): (_, Ipv4Addr) = into(be_u32::<_, NomError>)(i)?;
        if !(multicast_address.is_multicast() || multicast_address.is_unspecified()) {
            return Err(Err::Error(make_error(i, ErrorKind::Verify)));
        }
        let (i, mac_address) = into(fixed_slice::<6>)(i)?;
        let (i, friendly_name) = StringBuffer::parse(i)?;
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

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((
            self.knx_medium.gen(),
            self.device_status.gen(),
            self.knx_individual_address.gen(),
            self.project_installation_identifier.gen(),
            slice(self.serial_number),
            be_u32(self.routing_multicast_address.into()),
            slice(self.mac_address.bytes()),
            self.friendly_name.gen(),
        ))
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[repr(u8)]
pub enum KnxMedium {
    Tp1 = 0x02,
    Pl110 = 0x04,
    Rf = 0x10,
    KnxIp = 0x20,
}

impl From<KnxMedium> for u8 {
    fn from(f: KnxMedium) -> Self {
        f as u8
    }
}

impl KnxMedium {
    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        cf::be_u8((*self).into())
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub struct DeviceStatus {
    pub programming_mode: bool,
}

impl DeviceStatus {
    const BITMASK_PROGRAMMING_MODE: u8 = 0b0000_0001;

    pub const fn new(programming_mode: bool) -> Self {
        Self { programming_mode }
    }

    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], DeviceStatus> {
        use nm::*;
        let (i, value) = be_u8(i)?;
        let mode = (value & Self::BITMASK_PROGRAMMING_MODE) != 0;
        Ok((i, DeviceStatus::new(mode)))
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        let value = if self.programming_mode {
            Self::BITMASK_PROGRAMMING_MODE
        } else {
            0
        };
        be_u8(value)
    }
}

/// Bits 15 to 4 project number
/// Bits 3 to 0 installation number
#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
pub struct ProjectInstallationIdentifier(u16);

impl ProjectInstallationIdentifier {
    const BITMASK_PROJECT_NUMBER: u16 = 0xFFF0;
    const BITMASK_INSTALLATION_NUMBER: u16 = 0x000F;

    pub fn try_new(project: u16, installation: u16) -> Result<Self, ()> {
        if installation > Self::BITMASK_INSTALLATION_NUMBER {
            return Err(());
        }

        if project > (Self::BITMASK_PROJECT_NUMBER >> 1) {
            return Err(());
        }

        let data = (project << 4) | installation;
        Ok(Self(data))
    }

    pub fn project_number(&self) -> u16 {
        (self.0 & Self::BITMASK_PROJECT_NUMBER) >> Self::BITMASK_PROJECT_NUMBER.trailing_zeros()
    }

    pub fn installation_number(&self) -> u16 {
        self.0 & Self::BITMASK_INSTALLATION_NUMBER
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        cf::be_u16(self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct StringBuffer<const N: usize>([u8; N]);

impl<const N: usize> StringBuffer<N> {
    pub fn try_new(data: &str) -> Result<Self, StringCreationError> {
        if !Self::valid_iso8859_1(data) {
            return Err(StringCreationError::InvalidCharacters);
        }
        let data = data.as_bytes();
        if data.len() > N {
            return Err(StringCreationError::TooLong {
                max: N,
                actual: data.len(),
            });
        }
        let mut d = [0; N];
        d[..data.len()].copy_from_slice(data);
        Ok(Self(d))
    }

    fn valid_iso8859_1(s: &str) -> bool {
        s.chars().all(|c| c as u32 <= 0xFF)
    }

    pub(crate) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        use nm::*;
        map(fixed_slice::<N>, Self)(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        cf::slice(self.0)
    }
}

impl<const N: usize> From<StringBuffer<N>> for String {
    fn from(f: StringBuffer<N>) -> Self {
        String::from_utf8(f.0.to_vec()).unwrap_or_default()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringCreationError {
    TooLong { max: usize, actual: usize },
    InvalidCharacters,
}

impl std::fmt::Display for StringCreationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooLong { max, actual } => write!(
                f,
                "String may only contain {} characters but actually contains {}.",
                max, actual
            ),
            Self::InvalidCharacters => write!(f, "String contains non ISO8859-1 characters."),
        }
    }
}

impl std::error::Error for StringCreationError {}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_DI: [u8; 52] = [
        0x02, // knx medium
        0x01, // device status
        0x11, 0x00, // knx individual address
        0x00, 0x11, // project installation ID
        0x00, 0x01, 0x11, 0x11, 0x11, 0x11, // serial number
        224, 0, 23, 12, // routing multicast address
        0x45, 0x49, 0x42, 0x6E, 0x65, 0x74, // mac address
        b'M', b'Y', b'H', b'O', b'M', b'E', // friendly name "MYHOME"
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, // remainder of friendly name filled with zeros
    ];

    fn make_di() -> DeviceInfo {
        DeviceInfo {
            knx_medium: KnxMedium::Tp1,
            device_status: DeviceStatus::new(true),
            knx_individual_address: Address::new(AddressKind::Individual, 0x11, 0x00),
            serial_number: [0x00, 0x01, 0x11, 0x11, 0x11, 0x11],
            project_installation_identifier: ProjectInstallationIdentifier::try_new(0x001, 0x1)
                .unwrap(),
            routing_multicast_address: Ipv4Addr::new(224, 0, 23, 12),
            mac_address: MacAddress::new([0x45, 0x49, 0x42, 0x6E, 0x65, 0x74]),
            friendly_name: StringBuffer::try_new("MYHOME").unwrap(),
        }
    }

    #[test]
    fn parse_device_info() {
        let (rem, actual) = DeviceInfo::parse(&TEST_DATA_DI).unwrap();

        assert_eq!(0, rem.len());
        assert_eq!(make_di(), actual);
    }

    #[test]
    fn gen_device_info() {
        let to_serialize = make_di();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_DI.len() as u64);
        assert_eq!(&TEST_DATA_DI[..], &actual[..]);
    }
}
