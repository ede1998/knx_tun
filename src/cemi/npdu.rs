use crate::{
    address::{Address, AddressKind},
    snack::*,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SequenceNumber(u8);

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
pub enum Tpdu {
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
pub enum Apdu {
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
pub struct GroupData {
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

    pub const fn with_small_payload(payload: u8) -> Self {
        let mut data = [0; 14];
        data[0] = payload;
        Self {
            data,
            length: Length::SixBitOrLess,
        }
    }

    pub fn with_payload(payload: &[u8]) -> Result<Self, ()> {
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

    pub fn get(&self) -> &[u8] {
        match self.length {
            Length::SixBitOrLess => &self.data[..=0],
            Length::Bytes(len) => &self.data[..len.into()],
        }
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Length {
    SixBitOrLess,
    Bytes(u8),
}
