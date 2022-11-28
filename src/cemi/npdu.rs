use cookie_factory::BackToTheBuffer;

use crate::{address::AddressKind, snack::*};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Npdu(pub(crate) Tpdu);

impl Npdu {
    pub(crate) fn parse(i: In, is_address_zero: bool, address_kind: AddressKind) -> IResult<Self> {
        use nm::*;
        let (i, tpdu) =
            length_value_offset(be_u8, 1, |i| Tpdu::parse(i, is_address_zero, address_kind))(i)?;

        Ok((i, Npdu(tpdu)))
    }

    pub(crate) fn gen<'a, W: Write + BackToTheBuffer + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        back_to_the_buffer(
            1,
            move |buf| gen(self.0.gen(), buf),
            move |buf, len| {
                gen_simple(
                    be_u8(
                        (len - 1)
                            .try_into()
                            .map_err(|_| GenError::BufferTooSmall(8))?,
                    ),
                    buf,
                )
            },
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SequenceNumber(U4);

impl SequenceNumber {
    pub fn new(num: U4) -> Self {
        Self(num)
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
    pub(crate) fn parse(i: In, is_address_zero: bool, address_kind: AddressKind) -> IResult<Self> {
        use nm::*;

        enum Either<L, R> {
            Left(L),
            Right(R),
        }

        bits(|i: (In, usize)| {
            let (i, is_control) = bool(i)?;
            let (i, has_sequence) = bool(i)?;
            let (i, maybe_sequence) = map_res(U4::parse, |n| {
                if has_sequence {
                    Ok(Either::Left(SequenceNumber::new(n)))
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
                    U4::_0 => Ok(match address_kind {
                        AddressKind::Individual => parse_apdu(i, Self::DataIndividual)?,
                        AddressKind::Group => {
                            if is_address_zero {
                                parse_apdu(i, Self::DataBroadcast)?
                            } else {
                                parse_apdu(i, Self::DataGroup)?
                            }
                        }
                    }),
                    U4::_1 => bytes(map(Apdu::parse, Self::DataTagGroup))(i),
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
        move |out| match self {
            Tpdu::DataTagGroup(apdu) => apdu.gen(U6::_1)(out),
            Tpdu::DataBroadcast(apdu) | Tpdu::DataGroup(apdu) | Tpdu::DataIndividual(apdu) => {
                apdu.gen(U6::_0)(out)
            }
            Tpdu::DataConnected(seq, apdu) => apdu.gen(U2::_1.chain(seq.0))(out),
            Tpdu::Connect => be_u8(0b1000_0000)(out),
            Tpdu::Disconnect => be_u8(0b1000_0001)(out),
            Tpdu::Ack(seq) => bits([U2::_3.into(), seq.0.into(), U2::_2.into()])(out),
            Tpdu::Nak(seq) => bits([U2::_3.into(), seq.0.into(), U2::_3.into()])(out),
        }
    }
}

mod apdu_prefix_kind {
    use crate::snack::U4;

    pub const GROUP_VALUE_READ: U4 = U4::_0;
    pub const GROUP_VALUE_RESPONSE: U4 = U4::_1;
    pub const GROUP_VALUE_WRITE: U4 = U4::_2;
}

// TODO: split this type: only specific stuff belongs together,
// i.e. application layer GroupValue{Read,Response,Write} requires
// transport layer DataGroup
// and other application layer stuff requires other transport layer
// option
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Apdu {
    //Whatever,
    GroupValueRead,
    GroupValueResponse(GroupData),
    GroupValueWrite(GroupData),
}

impl Apdu {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use apdu_prefix_kind::*;
        use nm::*;
        let (i, (apdu_prefix_kind, maybe_apdu_suffix_kind)) =
            bits(preceded(bit_u8(6), pair(U4::parse, U6::parse)))(i)?;

        let parse_group_data = |i| GroupData::parse(maybe_apdu_suffix_kind, i);

        let (i, apdu) = match apdu_prefix_kind {
            GROUP_VALUE_READ => (i, Self::GroupValueRead),
            GROUP_VALUE_RESPONSE => map(parse_group_data, Self::GroupValueResponse)(i)?,
            GROUP_VALUE_WRITE => map(parse_group_data, Self::GroupValueWrite)(i)?,
            _ => unimplemented!("Apdu other cases"),
            //_ => map(rest, |_| Self::Whatever)(i)?,
        };

        nm::eof(i)?;
        Ok((i, apdu))
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self, prefix_bits: U6) -> impl SerializeFn<W> + 'a {
        use apdu_prefix_kind::*;
        use cf::*;
        move |out| match self {
            Apdu::GroupValueRead => {
                bits([prefix_bits.into(), GROUP_VALUE_READ.into(), bits::u8(6, 0)])(out)
            }
            Apdu::GroupValueResponse(gd) => pair(
                bits([prefix_bits.into(), GROUP_VALUE_RESPONSE.into()]),
                gd.gen(None),
            )(out),
            Apdu::GroupValueWrite(gd) => pair(
                bits([prefix_bits.into(), GROUP_VALUE_WRITE.into()]),
                gd.gen(None),
            )(out),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct GroupData {
    data: [u8; 14],
    length: Length,
}

#[derive(Clone, Copy, Debug)]
pub struct PayloadTooLarge;

impl GroupData {
    pub(crate) fn parse(remainder: U6, i: In) -> IResult<Self> {
        use nm::*;
        let (i, payload) = rest(i)?;
        let data = if payload.is_empty() {
            Self::with_small_payload(remainder)
        } else {
            Self::with_payload(payload)
                .map_err(|_| Err::Error(make_error(i, NomErrorKind::TooLarge)))?
        };
        Ok((i, data))
    }

    pub(crate) fn gen<'a, W: Write + 'a>(
        &'a self,
        prefix_bits: Option<U2>,
    ) -> impl SerializeFn<W> + 'a {
        use cf::*;
        let prefix_bits = prefix_bits.unwrap_or(U2::_0);
        move |out| match self.length {
            Length::SixBitOrLess => bits([prefix_bits.into(), bits::u8(6, self.data[0])])(out),
            Length::Bytes(len) => slice(&self.data[..len.into()])(out),
        }
    }

    pub const fn with_small_payload(payload: U6) -> Self {
        let mut data = [0; 14];
        data[0] = payload.as_u8();
        Self {
            data,
            length: Length::SixBitOrLess,
        }
    }

    pub fn with_payload(payload: &[u8]) -> Result<Self, PayloadTooLarge> {
        match payload.len().try_into().unwrap_or(u8::MAX) {
            15.. => Err(PayloadTooLarge),
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
