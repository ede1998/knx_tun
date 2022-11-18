use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::{
    address::{Address, AddressKind, RawAddress},
    snack::*,
};

use super::npdu::Tpdu;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct LData {
    pub control_1: Control1,
    pub control_2: Control2,
    pub source: Address,
    pub destination: RawAddress,
    pub tpdu: Tpdu,
}

impl LData {
    pub fn new(source: Address, destination: Address, tpdu: Tpdu) -> Self {
        Self {
            control_1: Default::default(),
            control_2: Control2::new(destination.kind()),
            source,
            destination: destination.raw(),
            tpdu,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(stringify!(LData), |i| {
            let (i, (control_1, control_2)) = pair(Control1::parse, Control2::parse)(i)?;
            let (i, source) = Address::parse(i, AddressKind::Individual)?;
            let (i, destination) = RawAddress::parse(i)?;
            let (i, tpdu) = length_value_offset(be_u8, 1, |i| {
                Tpdu::parse(i, destination.is_zero(), control_2.destination_address)
            })(i)?;

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

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((
            self.control_1.gen(),
            self.control_2.gen(),
            self.source.gen(),
            self.destination.gen(),
            // missing npdu in between here somewhere -> some length is missing, maybe 2 lengths...
            self.tpdu.gen(),
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Control1 {
    pub is_standard_frame: bool,
    pub do_not_repeat: bool,
    pub is_normal_broadcast: bool,
    pub priority: Priority,
    pub acknowledge_requested: bool,
    pub had_transmission_error: bool,
}

impl Default for Control1 {
    fn default() -> Self {
        Self {
            is_standard_frame: true,
            do_not_repeat: false,
            is_normal_broadcast: true,
            priority: Default::default(),
            acknowledge_requested: false,
            had_transmission_error: false,
        }
    }
}

impl Control1 {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(Control1),
            map(
                bits(tuple((bool, bool, bool, bool, Priority::parse, bool, bool))),
                |(sf, _, no_repeat, bc, priority, ack_req, err)| Self {
                    is_standard_frame: sf,
                    do_not_repeat: no_repeat,
                    is_normal_broadcast: bc,
                    priority,
                    acknowledge_requested: ack_req,
                    had_transmission_error: err,
                },
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::{bits, bits::bool};
        bits([
            bool(self.is_standard_frame),
            bool(false),
            bool(self.do_not_repeat),
            bool(self.is_normal_broadcast),
            self.priority.into(),
            bool(self.acknowledge_requested),
            bool(self.had_transmission_error),
        ])
    }
}

#[derive(
    Debug, Clone, Copy, IntoPrimitive, Default, TryFromPrimitive, PartialEq, Eq, PartialOrd, Ord,
)]
#[repr(u8)]
pub enum Priority {
    /// Mandatory for long Frames, burst traffic...
    Low = 0b11,
    /// Default for short Frames
    #[default]
    Normal = 0b01,
    /// Reserved for urgent Frames
    Urgent = 0b10,
    /// Reserved for high priority, system configuration and management procedures
    System = 0b00,
}

impl Priority {
    pub(crate) fn parse(i: (In, usize)) -> IBitResult<Self> {
        use nm::*;
        context(
            stringify!(Priority),
            map(U2::parse, |b| {
                u8::from(b).try_into().expect(
                "Failed to convert u2 to priority. This should have been an infalliable operation.",
            )
            }),
        )(i)
    }
}

impl From<Priority> for cf::Bits {
    fn from(p: Priority) -> Self {
        cf::bits::u8(2, p.into())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Control2 {
    pub destination_address: AddressKind,
    pub hop_count: U3,
    pub frame_format: FrameFormat,
}

impl Control2 {
    pub fn new(kind: AddressKind) -> Self {
        Self {
            destination_address: kind,
            hop_count: U3::MAX,
            frame_format: Default::default(),
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(Control2),
            map(
                bits(tuple((bool, U3::parse, FrameFormat::parse))),
                |(is_destination_group, hop_count, frame_format)| Self {
                    destination_address: if is_destination_group {
                        AddressKind::Group
                    } else {
                        AddressKind::Individual
                    },
                    hop_count,
                    frame_format,
                },
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::{bits, bits::bool};
        bits([
            bool(self.destination_address == AddressKind::Group),
            self.hop_count.into(),
            self.frame_format.into(),
        ])
    }
}

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum FrameFormat {
    #[default]
    Standard,
    Lte(U2),
}

impl FrameFormat {
    pub(crate) fn parse(i: (In, usize)) -> IBitResult<Self> {
        use nm::*;
        context(
            stringify!(FrameFormat),
            map(
                tuple((bool, bool, U2::parse)),
                |(_, is_lte_frame, number)| {
                    if is_lte_frame {
                        Self::Lte(number)
                    } else {
                        Self::Standard
                    }
                },
            ),
        )(i)
    }
}

impl From<FrameFormat> for cf::Bits {
    fn from(f: FrameFormat) -> Self {
        cf::bits::u8(
            4,
            match f {
                FrameFormat::Standard => 0,
                FrameFormat::Lte(number) => 0b0100 | u8::from(number),
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serialize_control1() {
        let ctrl1 = Control1 {
            is_standard_frame: true,
            do_not_repeat: false,
            is_normal_broadcast: true,
            priority: Priority::Normal,
            acknowledge_requested: false,
            had_transmission_error: false,
        };

        let (serialized, len) = cookie_factory::gen(ctrl1.gen(), vec![]).unwrap();

        println!("{:?}", serialized);
        assert_eq!(serialized.len(), 1, "Wrong length.");
        assert_eq!(len, 1, "Wrong length in result.");
        let byte = serialized[0];
        assert!(byte & 0x80 != 0, "Wrong value for is_standard_frame.");
        assert!(byte & 0x40 == 0, "Wrong value for unused bit.");
        assert!(byte & 0x20 == 0, "Wrong value for do_not_repeat.");
        assert!(byte & 0x10 != 0, "Wrong value for is_normal_broadcast.");
        assert_eq!(
            Priority::try_from((byte & 0x0C) >> 2),
            Ok(Priority::Normal),
            "Wrong value for priority."
        );
        assert!(byte & 0x2 == 0, "Wrong value for acknowledge_requested.");
        assert!(byte & 0x1 == 0, "Wrong value for had_transmission_error.");
    }

    #[test]
    fn parse_control1() {
        #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
        const CONTROL_1: u8 = 0b1_0_1_1_00_0_0;
        let expected = Control1 {
            is_standard_frame: true,
            do_not_repeat: true,
            is_normal_broadcast: true,
            priority: Priority::System,
            acknowledge_requested: false,
            had_transmission_error: false,
        };

        let (i, ctrl1) = Control1::parse(&[CONTROL_1]).unwrap();

        assert!(i.is_empty(), "Didn't parse enough data.");
        assert_eq!(ctrl1, expected, "Wrong field in Control1.");
    }

    #[test]
    fn serialize_control2() {
        let ctrl2 = Control2 {
            destination_address: AddressKind::Group,
            hop_count: U3::unwrap(5),
            frame_format: FrameFormat::Lte(U2::unwrap(3)),
        };

        let (serialized, len) = cookie_factory::gen(ctrl2.gen(), vec![]).unwrap();

        println!("{:?}", serialized);
        assert_eq!(serialized.len(), 1, "Wrong length.");
        assert_eq!(len, 1, "Wrong length in result.");
        let byte = serialized[0];
        assert!(byte & 0x80 != 0, "Wrong value for destination_address.");
        assert_eq!((byte & 0x70) >> 4, 5, "Wrong value for hop_count.");
        assert!(byte & 0x0F == 0b0111, "Wrong value for frame_format.");
    }

    #[test]
    fn parse_control2() {
        #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
        const CONTROL_2: u8 = 0b0_011_0110;
        let expected = Control2 {
            destination_address: AddressKind::Individual,
            hop_count: U3::unwrap(3),
            frame_format: FrameFormat::Lte(U2::unwrap(2)),
        };

        let (i, ctrl2) = Control2::parse(&[CONTROL_2]).unwrap();

        assert!(i.is_empty(), "Didn't parse enough data.");
        assert_eq!(ctrl2, expected, "Wrong field in Control2.");
    }
}
