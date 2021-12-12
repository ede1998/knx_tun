use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    address::{Address, AddressKind},
    snack::*,
};

use super::npdu::Tpdu;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LData {
    pub control_1: u8,
    pub control_2: u8,
    pub source: Address,
    pub destination: Address,
    pub tpdu: Tpdu,
}

impl LData {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(stringify!(LData), |i| {
            let (i, (control_1, control_2)) = pair(be_u8, be_u8)(i)?;
            let (i, source) = Address::parse(i, AddressKind::Individual)?;
            let dest_addr_kind = if (control_2 >> 7) == 1 {
                AddressKind::Group
            } else {
                AddressKind::Individual
            };
            let (i, destination) = Address::parse(i, dest_addr_kind)?;
            let (i, tpdu) = length_value_offset(be_u8, 1, |i| Tpdu::parse(i, destination))(i)?;

            Ok((
                i,
                Self {
                    control_1,
                    control_2,
                    source,
                    destination,
                    tpdu,
                },
            ))
        })(i)
    }

    /// Sensible default for CTRL_1 for now.
    #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
    const CONTROL_1: u8 = 0b1_0_1_1_00_0_0;
    /// Sensible default for CTRL_2 for now.
    #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
    const CONTROL_2: u8 = 0b1_111_0000;

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        let control2 = match self.destination.kind() {
            AddressKind::Group => self.control_2 | 0b1000_0000,
            AddressKind::Individual => self.control_2 & !0b1000_0000,
        };
        tuple((
            be_u8(self.control_1),
            be_u8(control2),
            self.source.gen(),
            self.destination.gen(),
        ))
    }
}