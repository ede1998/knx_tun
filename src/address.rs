use crate::snack::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub enum AddressKind {
    Individual,
    Group,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct Address {
    kind: AddressKind,
    subnet: u8,
    device: u8,
}

impl Address {
    pub const fn new(kind: AddressKind, subnet: u8, device: u8) -> Self {
        Self {
            kind,
            subnet,
            device,
        }
    }

    pub fn area(&self) -> u8 {
        self.subnet >> 4
    }

    pub fn line(&self) -> u8 {
        self.subnet & 0xF
    }

    pub fn device(&self) -> u8 {
        self.device
    }

    pub fn kind(&self) -> AddressKind {
        self.kind
    }

    pub(crate) fn parse(i: In, kind: AddressKind) -> IResult<Self> {
        use nm::*;
        let (i, (subnet, device)) = context("KNX address", tuple((be_u8, be_u8)))(i)?;
        Ok((i, Self::new(kind, subnet, device)))
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((be_u8(self.subnet), be_u8(self.device)))
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
