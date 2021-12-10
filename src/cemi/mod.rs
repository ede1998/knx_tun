use crate::{
    address::{Address, AddressKind},
    snack::*,
};
use nom_derive::NomBE;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
enum MessageCode {
    LBusmonInd = 0x2B,
    LDataCon = 0x2E,
    LDataInd = 0x29,
    LDataReq = 0x11,
    LPollDataCon = 0x25,
    LPollDataReq = 0x13,
    LRawCon = 0x2F,
    LRawInd = 0x2D,
    LRawReq = 0x10,
    /// MFuncPropStateread.con
    MFuncPropCommandCon = 0xFA,
    MFuncPropCommandReq = 0xF8,
    MFuncPropStateReadReq = 0xF9,
    MPropInfoInd = 0xF7,
    MPropReadCon = 0xFB,
    MPropReadReq = 0xFC,
    MPropWriteCon = 0xF5,
    MPropWriteReq = 0xF6,
    MResetInd = 0xF0,
    MResetReq = 0xF1,
    TDataConnectedInd = 0x89,
    TDataConnectedReq = 0x41,
    TDataIndividualInd = 0x94,
    TDataIndividualReq = 0x4A,
}

impl From<MessageCode> for u8 {
    fn from(f: MessageCode) -> Self {
        f as _
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Cemi {
    pub header: CemiHeader,
    pub body: CemiBody,
}

impl Cemi {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        let (i, header) = CemiHeader::parse(i)?;
        let (i, body) = match header.message_code {
            MessageCode::LDataCon | MessageCode::LDataInd | MessageCode::LDataReq => {
                into(LData::parse)(i)?
            }
            _ => unimplemented!(),
        };
        Ok((i, Self { header, body }))
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct CemiHeader {
    pub message_code: MessageCode,
    pub additional_info: AdditionalInformation,
}

impl CemiHeader {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(CemiHeader),
            map(
                pair(MessageCode::parse, AdditionalInformation::parse),
                |(mc, ai)| Self {
                    message_code: mc,
                    additional_info: ai,
                },
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((be_u8(self.message_code.into()), self.additional_info.gen()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum CemiBody {
    LData(LData),
}

impl From<LData> for CemiBody {
    fn from(f: LData) -> Self {
        Self::LData(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct LData {
    control_1: u8,
    control_2: u8,
    source: Address,
    destination: Address,
    tpdu: Tpdu,
}

impl LData {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct AdditionalInformation;

impl AdditionalInformation {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        map(length_data(be_u8), |_| Self)(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        be_u8(0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct SequenceNumber(u8);

impl SequenceNumber {
    pub fn new(num: u8) -> Result<Self, ()> {
        if num <= 0b1111 {
            Ok(Self(num))
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Tpdu {
    DataBroadcast(Apdu),
    DataGroup(Apdu),
    DataTagGroup(Apdu),
    DataIndividual(Apdu),
    DataConnected(SequenceNumber, Apdu),
    Connect,
    Disconnect,
    Ack(SequenceNumber),
    Nak(SequenceNumber),
}

impl Tpdu {
    fn has_data(&self) -> bool {
        match self {
            Self::DataBroadcast(_)
            | Self::DataGroup(_)
            | Self::DataTagGroup(_)
            | Self::DataIndividual(_)
            | Self::DataConnected(_, _) => true,
            Self::Connect | Self::Disconnect | Self::Ack(_) | Self::Nak(_) => false,
        }
    }

    fn has_sequence(&self) -> bool {
        match self {
            Tpdu::DataBroadcast(_)
            | Tpdu::DataGroup(_)
            | Tpdu::DataTagGroup(_)
            | Tpdu::DataIndividual(_)
            | Tpdu::Connect
            | Tpdu::Disconnect => false,
            Tpdu::DataConnected(_, _) | Tpdu::Ack(_) | Tpdu::Nak(_) => true,
        }
    }
    pub(crate) fn parse(i: In, address: Address) -> IResult<Self> {
        use nm::*;

        enum Either<L, R> {
            Left(L),
            Right(R),
        }

        bits(|i: (In, usize)| {
            let (i, is_control) = bool(i)?;
            let (i, has_sequence) = bool(i)?;
            let (i, maybe_sequence) = map_res(bit_u8(4), |n| {
                if has_sequence {
                    Ok(Either::Left(SequenceNumber::new(n)?))
                } else {
                    Ok(Either::Right(n))
                }
            })(i)?;

            fn parse_apdu(i: (In, usize), f: impl FnOnce(Apdu) -> Tpdu) -> IBitResult<Tpdu> {
                let (i, apdu) = bytes(Apdu::parse)((i.0, 0))?;
                Ok((i, f(apdu)))
            }

            match (is_control, maybe_sequence) {
                (true, Either::Left(sequence)) => {
                    let (i, tag) = bit_u8(2)(i)?;
                    match tag {
                        0b10 => Ok((i, Self::Ack(sequence))),
                        0b11 => Ok((i, Self::Nak(sequence))),
                        _ => Err(Err::Error(make_error(i, NomErrorKind::Switch))),
                    }
                }
                (true, Either::Right(_)) => {
                    let (i, tag) = bit_u8(2)(i)?;
                    match tag {
                        0b00 => Ok((i, Self::Connect)),
                        0b01 => Ok((i, Self::Disconnect)),
                        _ => Err(Err::Error(make_error(i, NomErrorKind::Switch))),
                    }
                }
                (false, Either::Right(tag)) => match tag {
                    0 => Ok(match address.kind() {
                        AddressKind::Individual => parse_apdu(i, Self::DataIndividual)?,
                        AddressKind::Group => {
                            if address.is_zero() {
                                parse_apdu(i, Self::DataBroadcast)?
                            } else {
                                parse_apdu(i, Self::DataGroup)?
                            }
                        }
                    }),
                    1 => bytes(map(Apdu::parse, Self::DataTagGroup))(i),
                    _ => Err(Err::Error(make_error(i, NomErrorKind::Switch))),
                },
                (false, Either::Left(sequence)) => {
                    Ok(parse_apdu(i, |apdu| Self::DataConnected(sequence, apdu))?)
                }
            }
        })(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        be_u8(0)
    }
}

mod apdu_prefix_kind {
    pub const GROUP_VALUE_READ: u8 = 0b0000;
    pub const GROUP_VALUE_RESPONSE: u8 = 0b0001;
    pub const GROUP_VALUE_WRITE: u8 = 0b0010;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Apdu {
    Whatever,
    GroupValueRead,
    GroupValueResponse(GroupData),
    GroupValueWrite(GroupData),
}

impl Apdu {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use apdu_prefix_kind::*;
        use nm::*;
        let (i, (apdu_prefix_kind, maybe_apdu_suffix_kind)) =
            bits(preceded(bit_u8(6), pair(bit_u8(4), bit_u8(6))))(i)?;

        let parse_group_data = |i| GroupData::parse(maybe_apdu_suffix_kind, i);

        let (i, apdu) = match apdu_prefix_kind {
            GROUP_VALUE_READ => (i, Self::GroupValueRead),
            GROUP_VALUE_RESPONSE => map(parse_group_data, Self::GroupValueResponse)(i)?,
            GROUP_VALUE_WRITE => map(parse_group_data, Self::GroupValueWrite)(i)?,
            _ => map(rest, |_| Self::Whatever)(i)?,
        };

        nm::eof(i)?;
        Ok((i, apdu))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct GroupData {
    data: [u8; 14],
    length: Length,
}

impl GroupData {
    pub(crate) fn parse(remainder: u8, i: In) -> IResult<Self> {
        use nm::*;
        let data = if i.is_empty() {
            Self::with_small_payload(remainder)
        } else {
            let (i, payload) = rest(i)?;
            Self::with_payload(payload)
                .map_err(|_| Err::Error(make_error(i, NomErrorKind::TooLarge)))?
        };
        Ok((i, data))
    }

    fn with_small_payload(payload: u8) -> Self {
        let mut data = [0; 14];
        data[0] = payload;
        Self {
            data,
            length: Length::SixBitOrLess,
        }
    }

    fn with_payload(payload: &[u8]) -> Result<Self, ()> {
        match payload.len().try_into().unwrap_or(u8::MAX) {
            15.. => Err(()),
            len => {
                let mut data = [0; 14];
                data[..payload.len()].copy_from_slice(payload);
                Ok(Self {
                    data,
                    length: Length::Bytes(len),
                })
            }
        }
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Length {
    SixBitOrLess,
    Bytes(u8),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[rustfmt::skip]
    const CEMI_IND_GROUP_VALUE_WRITE: [u8; 11] = [
        0x29, // message code L_DATA.ind
        0x00, // Additional info len = 0 bytes
        0xbc, // CTRL1
        0xe0, // CTRL 2
        0x11, 0x0b, // source device address
        0x69, 0x01, // target address (group)
        0x01, // length 1
        0b000000_00, // TPCI: T_Data_Group-PDU (destination_address <> 0), APCI: first 2 bits of tag
        #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
        0b10_000001, // APCI: A_GroupValue_Write-PDU, data: 1
    ];

    const STRUCT_CEMI_IND_GROUP_VALUE_WRITE: Cemi = Cemi {
        header: CemiHeader {
            message_code: MessageCode::LDataInd,
            additional_info: AdditionalInformation,
        },
        body: CemiBody::LData(LData {
            control_1: 0xbc,
            control_2: 0xe0,
            source: Address::new(AddressKind::Individual, 0x11, 0x0b),
            destination: Address::new(AddressKind::Group, 0x69, 0x01),
            tpdu: Tpdu::DataGroup(Apdu::GroupValueWrite(GroupData {
                data: [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                length: Length::SixBitOrLess,
            })),
        }),
    };

    #[test]
    fn parse_cemi() {
        let (rem, actual) = Cemi::parse(&CEMI_IND_GROUP_VALUE_WRITE).unwrap();

        assert_eq!(0, rem.len());
        assert_eq!(actual, STRUCT_CEMI_IND_GROUP_VALUE_WRITE);
    }
}
