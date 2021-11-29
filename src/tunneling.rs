use std::{borrow::Cow, convert::TryFrom, marker::PhantomData};

use nom_derive::NomBE;

use crate::snack::*;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectionHeader<T> {
    pub communication_channel_id: u8,
    pub sequence_counter: u8,
    data: u8,
    data_type: PhantomData<T>,
}

impl<T> ConnectionHeader<T> {
    pub fn new(communication_channel_id: u8, sequence_counter: u8, data: T) -> Self
    where
        T: Into<u8>,
    {
        Self {
            communication_channel_id,
            sequence_counter,
            data: data.into(),
            data_type: Default::default(),
        }
    }
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(ConnectionHeader),
            length_value_incl(
                be_u8,
                map(tuple((be_u8, be_u8, be_u8)), |(chan, counter, data)| Self {
                    communication_channel_id: chan,
                    sequence_counter: counter,
                    data,
                    data_type: Default::default(),
                }),
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W>(&'a self) -> impl SerializeFn<W> + 'a
    where
        W: Write + 'a,
    {
        use cf::*;
        length_data_incl(
            1,
            tuple((
                be_u8(self.communication_channel_id),
                be_u8(self.sequence_counter),
                be_u8(self.data),
            )),
        )
    }

    pub fn data(&self) -> Result<T, u8>
    where
        T: TryFrom<u8>,
    {
        T::try_from(self.data).map_err(|_| self.data)
    }
}

impl ConnectionHeader<()> {
    pub fn reserved(communication_channel_id: u8, sequence_counter: u8) -> Self {
        Self {
            communication_channel_id,
            sequence_counter,
            data: 0,
            data_type: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct TunnelingRequest<'a> {
    pub header: ConnectionHeader<()>,
    pub cemi: Cow<'a, [u8]>,
}

impl<'a> TunnelingRequest<'a> {
    pub fn new(
        communication_channel_id: u8,
        sequence_counter: u8,
        cemi: Cow<'a, [u8]>,
    ) -> Self {
        Self {
            header: ConnectionHeader::reserved(communication_channel_id, sequence_counter),
            cemi,
        }
    }

    pub(crate) fn parse(i: In<'a>) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(TunnelingRequest),
            map(tuple((ConnectionHeader::parse, rest)), |(header, cemi)| {
                Self {
                    header,
                    cemi: cemi.into(),
                }
            }),
        )(i)
    }

    pub(crate) fn gen<W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((self.header.gen(), slice(self.cemi.as_ref())))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum TunnelingAckState {
    NoError = 0x00,
    // TODO find other allowed states
}

impl From<TunnelingAckState> for u8 {
    fn from(f: TunnelingAckState) -> Self {
        f as _
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct TunnelingAck(pub ConnectionHeader<TunnelingAckState>);

impl TunnelingAck {
    pub fn new(
        communication_channel_id: u8,
        sequence_counter: u8,
        data: TunnelingAckState,
    ) -> Self {
        Self(ConnectionHeader::new(
            communication_channel_id,
            sequence_counter,
            data,
        ))
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(stringify!(TunnelingAck), map(ConnectionHeader::parse, Self))(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        self.0.gen()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_TACK: [u8; 4] = [
        0x04, // length
        0x15, // communication_channel_id
        0x02, // sequence counter
        0x00, // status
    ];

    fn make_test_tack() -> TunnelingAck {
        TunnelingAck::new(21, 2, TunnelingAckState::NoError)
    }

    #[test]
    fn parse_tunneling_ack() {
        let (rem, actual) = TunnelingAck::parse(&TEST_DATA_TACK).unwrap();

        assert_eq!(0, rem.len());
        let expected = make_test_tack();
        assert_eq!(expected, actual);
    }

    #[test]
    fn gen_tunneling_ack() {
        let to_serialize = make_test_tack();
        let (actual, len) = cookie_factory::gen(to_serialize.gen(), vec![]).unwrap();
        assert_eq!(len, TEST_DATA_TACK.len() as u64);
        assert_eq!(&TEST_DATA_TACK[..], &actual[..]);
    }
}
