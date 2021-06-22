use nom_derive::*;

use crate::snack::*;

mod device_info;
mod service_family;

use device_info::DeviceInfo;
use service_family::ServiceFamily;

#[derive(Clone, Debug, Copy, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum DescriptionType {
    /// Device information e.g. KNX medium.
    DeviceInfo = 0x01,
    /// Service families supported by the device.
    SuppSvcFamilies = 0x02,
    /// IP configuration
    IpConfiguration = 0x03,
    /// Current configuration
    IpCurConfig = 0x04,
    /// KNX addresses
    KnxAddresses = 0x05,
    /// DIB structure for further data defined by device manufacturer.
    ManufacturerData = 0xFE,
}

impl From<DescriptionType> for u8 {
    fn from(f: DescriptionType) -> Self {
        f as u8
    }
}

impl DescriptionType {
    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        cf::be_u8((*self).into())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Dib {
    DeviceInfo(DeviceInfo),
    SupportedServiceFamilies(Vec<ServiceFamily>),
    IpConfiguration,
    IpCurrentConfiguration,
    KnxAddresses,
    ManufacturerData,
}

impl From<DeviceInfo> for Dib {
    fn from(f: DeviceInfo) -> Self {
        Self::DeviceInfo(f)
    }
}

impl From<Vec<ServiceFamily>> for Dib {
    fn from(f: Vec<ServiceFamily>) -> Self {
        Self::SupportedServiceFamilies(f)
    }
}

impl Dib {
    pub(crate) fn parse(i: &[u8]) -> IResult<Self> {
        use nm::*;
        context(
            "DIB",
            length_value_incl(be_u8, |i| {
                let (i, con_type) = DescriptionType::parse(i)?;
                match con_type {
                    DescriptionType::DeviceInfo => into(DeviceInfo::parse)(i),
                    DescriptionType::SuppSvcFamilies => into(all0(ServiceFamily::parse))(i),
                    _ => unimplemented!(),
                }
            }),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        use DescriptionType::*;
        length_data_incl(1, move |out| match self {
            Self::DeviceInfo(d) => tuple((DeviceInfo.gen(), d.gen()))(out),
            Self::SupportedServiceFamilies(f) => {
                tuple((SuppSvcFamilies.gen(), all(f.iter().map(ServiceFamily::gen))))(out)
            }
            _ => todo!(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use service_family::ServiceFamilyId;

    const TEST_DATA_DIB: [u8; 8] = [
        0x8,  // length
        0x02, // type supported families
        0x02, 0x01, // family version tuple core
        0x03, 0x02, // family version tuple device mgmt
        0x04, 0x01, // family version tuple tunneling
    ];

    fn make_dib() -> Dib {
        Dib::SupportedServiceFamilies(vec![
            ServiceFamily::new(ServiceFamilyId::Core, 1),
            ServiceFamily::new(ServiceFamilyId::DeviceManagement, 2),
            ServiceFamily::new(ServiceFamilyId::Tunnelling, 1),
        ])
    }

    #[test]
    fn parse_dib() {
        let (rem, actual) = Dib::parse(&TEST_DATA_DIB).unwrap();

        assert_eq!(0, rem.len());
        assert_eq!(make_dib(), actual);
    }

    #[test]
    fn parse_empty_dib() {
        let (rem, actual) = Dib::parse(&[2, 2]).unwrap();

        assert_eq!(0, rem.len());
        assert_eq!(Dib::SupportedServiceFamilies(Vec::new()), actual);
    }

    #[test]
    fn gen_device_info() {
        let to_serialize = make_dib();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_DIB.len() as u64);
        assert_eq!(&TEST_DATA_DIB[..], &actual[..]);
    }
}
