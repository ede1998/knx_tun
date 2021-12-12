use crate::snack::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum AddressKind {
    Individual,
    Group,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct RawAddress {
    pub subnet: u8,
    pub device: u8,
}

impl RawAddress {
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

    pub const fn is_zero(&self) -> bool {
        self.subnet == 0 && self.device == 0
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

    pub fn area(&self) -> u8 {
        self.address.subnet >> 4
    }

    pub fn line(&self) -> u8 {
        self.address.subnet & 0xF
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
            sep = sep
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
}
