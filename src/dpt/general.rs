use crate::cemi::GroupData;

pub trait DataPointType: Sized {
    const MAIN_NUMBER: u16;
    const SUB_NUMBER: u16;
    type ParseError<'a>
    where
        Self: 'a;

    fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>>;

    fn to_data(&self) -> GroupData;

    fn id() -> String {
        format!("{}.{:03}", Self::MAIN_NUMBER, Self::SUB_NUMBER)
    }
}
