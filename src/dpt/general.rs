use crate::snack::*;

pub trait DataPointType: Sized {
    const MAIN_NUMBER: u16;
    const SUB_NUMBER: u16;
    const LESS_THAN_A_BYTE: bool = false;
    fn parse(i: In) -> IResult<Self>;

    fn gen_into<W: Write>(&self, out: W) -> Result<(W, u64), GenError>;

    fn id() -> String {
        format!("{}.{:03}", Self::MAIN_NUMBER, Self::SUB_NUMBER)
    }
}
