use std::convert::{TryFrom, TryInto};

use nom::error::FromExternalError;

use crate::snack::*;

#[derive(Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct ConnectionHeader<T: Into<u8> + TryFrom<u8>> {
    pub communication_channel_id: u8,
    pub sequence_counter: u8,
    pub data: T,
}

impl<T> ConnectionHeader<T>
where
    T: Into<u8> + TryFrom<u8>,
{
    pub fn new(communication_channel_id: u8, sequence_counter: u8, data: T) -> Self {
        Self {
            communication_channel_id,
            sequence_counter,
            data,
        }
    }

    pub(crate) fn parse<'a>(i: In<'a>) -> IResult<'a, Self>
    where
        nm::Error<In<'a>>: FromExternalError<In<'a>, T::Error>,
    {
        use nm::*;
        context(
            stringify!(ConnectionHeader),
            length_value_incl(
                be_u8,
                map_res(tuple((be_u8, be_u8, be_u8)), |(chan, counter, data)| {
                    Ok(Self::new(chan, counter, data.try_into()?))
                }),
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W>(&'a self) -> impl SerializeFn<W> + 'a
    where
        W: Write + 'a,
        T: Clone,
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
