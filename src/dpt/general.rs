use std::fmt::Display;

use crate::cemi::GroupData;

pub trait DataPointType: Sized {
    const ID: DataPointId;
    type ParseError<'a>
    where
        Self: 'a;

    fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>>;

    fn to_data(&self) -> GroupData;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DataPointId {
    pub main: u16,
    pub sub: u16,
}

impl DataPointId {
    pub const fn new(main: u16, sub: u16) -> Self {
        Self { main, sub }
    }
}

impl Display for DataPointId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}.{:03}", self.main, self.sub)
    }
}
