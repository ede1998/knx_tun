use nom::{number::streaming::be_u16, IResult};

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
    pub fn area(&self) -> u8 {
        self.subnet >> 4
    }

    pub fn line(&self) -> u8 {
        self.subnet & 0xF
    }

    pub fn device(&self) -> u8 {
        self.device
    }
}

impl From<Address> for u16 {
    fn from(addr: Address) -> Self {
        let Address { subnet, device, .. } = addr;
        u16::from_be_bytes([subnet, device])
    }
}

pub(crate) fn parse_address(i: &[u8], kind: AddressKind) -> IResult<&[u8], Address> {
    let (i, value) = be_u16(i)?;
    let [subnet, device] = value.to_be_bytes();
    let addr = Address {
        kind,
        device,
        subnet,
    };
    Ok((i, addr))
}
