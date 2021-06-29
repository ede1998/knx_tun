use nom_derive::{NomBE, Parse};

use crate::snack::*;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectionHeader<T> {
    pub communication_channel_id: u8,
    pub sequence_counter: u8,
    pub data: T,
}

impl<T> ConnectionHeader<T> {
    pub const fn new(communication_channel_id: u8, sequence_counter: u8, data: T) -> Self {
        Self {
            communication_channel_id,
            sequence_counter,
            data,
        }
    }
    pub(crate) fn parse<'a>(i: In<'a>) -> IResult<'a, Self>
    where
        T: Parse<In<'a>, nm::Error<In<'a>>>,
    {
        use nm::*;
        context(
            stringify!(ConnectionHeader),
            length_value_incl(
                be_u8,
                map(tuple((be_u8, be_u8, T::parse)), |(chan, counter, data)| {
                    Self::new(chan, counter, data)
                }),
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W>(&'a self) -> impl SerializeFn<W> + 'a
    where
        W: Write + 'a,
        T: Clone + Into<u8>,
    {
        use cf::*;
        length_data_incl(
            1,
            tuple((
                be_u8(self.communication_channel_id),
                be_u8(self.sequence_counter),
                be_u8(self.data.clone().into()),
            )),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct TunnelingRequest {
    pub header: ConnectionHeader<u8>,
    pub cemi: u8,
}

impl TunnelingRequest {
    pub const fn new(communication_channel_id: u8, sequence_counter: u8, cemi: u8) -> Self {
        Self {
            header: ConnectionHeader::new(communication_channel_id, sequence_counter, 0),
            cemi,
        }
    }

    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(TunnelingRequest),
            map(tuple((ConnectionHeader::parse, be_u8)), |(header, cemi)| {
                Self { header, cemi }
            }),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((self.header.gen(), be_u8(self.cemi)))
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
    pub const fn new(
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
