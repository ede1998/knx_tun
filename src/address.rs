use std::str::FromStr;

use crate::snack::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum AddressKind {
    Individual,
    Group,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub struct RawAddress {
    pub subnet: u8,
    pub device: u8,
}

impl RawAddress {
    pub const ZERO: RawAddress = RawAddress {
        device: 0,
        subnet: 0,
    };

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(RawAddress),
            map(pair(be_u8, be_u8), |(subnet, device)| Self {
                subnet,
                device,
            }),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        pair(be_u8(self.subnet), be_u8(self.device))
    }

    pub fn is_zero(&self) -> bool {
        *self == Self::ZERO
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Address {
    kind: AddressKind,
    address: RawAddress,
}

impl Address {
    pub const fn new(kind: AddressKind, subnet: u8, device: u8) -> Self {
        Self {
            kind,
            address: RawAddress { subnet, device },
        }
    }

    pub const fn group(area: U5, line: U3, device: u8) -> Self {
        Self {
            kind: AddressKind::Group,
            address: RawAddress {
                subnet: area.chain::<3, 8>(line).as_u8(),
                device,
            },
        }
    }

    pub const fn individual(area: U5, line: U3, device: u8) -> Self {
        Self {
            kind: AddressKind::Individual,
            address: RawAddress {
                subnet: area.chain::<3, 8>(line).as_u8(),
                device,
            },
        }
    }

    pub const fn from_raw(kind: AddressKind, address: RawAddress) -> Self {
        Self { kind, address }
    }

    pub const fn zero(kind: AddressKind) -> Self {
        Self {
            kind,
            address: RawAddress::ZERO,
        }
    }

    pub fn area(&self) -> U5 {
        U5::unwrap(self.address.subnet >> 3)
    }

    pub fn line(&self) -> U3 {
        U3::unwrap(self.address.subnet & 0b111)
    }

    pub fn device(&self) -> u8 {
        self.address.device
    }

    pub fn subnet(&self) -> u8 {
        self.address.subnet
    }

    pub fn kind(&self) -> AddressKind {
        self.kind
    }

    pub fn raw(&self) -> RawAddress {
        self.address
    }

    pub(crate) fn parse(i: In, kind: AddressKind) -> IResult<Self> {
        use nm::*;
        context(
            "KNX address",
            map(RawAddress::parse, |address| Self { address, kind }),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        self.address.gen()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AddressParseError {
    MissingComponents,
    TooManyComponent(usize),
    MixedSeparators,
    MissingSeparator,
    InvalidNumber(String),
}

impl FromStr for Address {
    type Err = AddressParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let is_group = s.contains('/');
        let is_individual = s.contains('.');
        let (kind, sep) = match (is_group, is_individual) {
            (true, false) => (AddressKind::Group, "/"),
            (false, true) => (AddressKind::Individual, "."),
            (true, true) => return Err(AddressParseError::MixedSeparators),
            (false, false) => return Err(AddressParseError::MissingSeparator),
        };
        let mut numbers = s.split(sep);

        let mut parse_number = |max_value: u8| -> Result<u8, AddressParseError> {
            let num = numbers.next().ok_or(AddressParseError::MissingComponents)?;
            let number = num
                .parse()
                .map_err(|_| AddressParseError::InvalidNumber(num.to_owned()))?;

            if number > max_value {
                Err(AddressParseError::InvalidNumber(num.to_owned()))
            } else {
                Ok(number)
            }
        };

        let area = parse_number(U5::MAX_U8)?;
        let line = parse_number(U3::MAX_U8)?;
        let device = parse_number(u8::MAX)?;

        let remainder = numbers.count();
        if remainder > 0 {
            return Err(AddressParseError::TooManyComponent(remainder));
        }

        Ok(Address {
            kind,
            address: RawAddress {
                subnet: (area << 3) | line,
                device,
            },
        })
    }
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sep = match self.kind {
            AddressKind::Group => '/',
            AddressKind::Individual => '.',
        };
        write!(
            f,
            "{}{sep}{}{sep}{}",
            self.area(),
            self.line(),
            self.device(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_ADDRESS: [u8; 2] = [0x11, 0x0C];
    const TEST_ADDRESS: Address = Address::new(AddressKind::Individual, 0x11, 0x0C);

    #[test]
    fn parse_address() {
        let (rem, actual) = Address::parse(&TEST_DATA_ADDRESS, AddressKind::Individual).unwrap();

        assert_eq!(0, rem.len());
        assert_eq!(TEST_ADDRESS, actual);
    }

    #[test]
    fn gen_address() {
        let (actual, len) = cookie_factory::gen(TEST_ADDRESS.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_ADDRESS.len() as u64);
        assert_eq!(&TEST_DATA_ADDRESS[..], &actual[..]);
    }

    #[test]
    fn stringify_address() {
        let (_, actual) = Address::parse(&[0x42, 0x13], AddressKind::Group).unwrap();
        assert_eq!(format!("{actual}"), "8/2/19");
    }

    #[test]
    fn parse_string() {
        let actual: Address = "8/2/19".parse().unwrap();
        let expected = Address::group(U5::_8, U3::_2, 19);
        assert_eq!(actual, expected, "{actual} != {expected}");

        let actual: Address = "28.4.149".parse().unwrap();
        assert_eq!(actual, Address::individual(U5::_28, U3::_4, 149));
    }
}
