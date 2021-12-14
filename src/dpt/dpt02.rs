use std::io::Write;

use cookie_factory::GenError;

use super::{dpt01::*, general::DataPointType};
use crate::snack::{self, IResult, In};

pub trait BaseFunction: DataPointType + Into<bool> + From<bool> + Copy {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Control<D: BaseFunction> {
    pub has_control: bool,
    pub function: D,
}

impl<D: BaseFunction> DataPointType for Control<D> {
    const MAIN_NUMBER: u16 = 2;
    const SUB_NUMBER: u16 = D::SUB_NUMBER;
    const LESS_THAN_A_BYTE: bool = true;

    fn parse(i: In) -> IResult<Self> {
        use snack::nm::*;
        map(
            bits(tuple((bit_u8(6), bool, bool))),
            |(_, has_control, b)| Self {
                has_control,
                function: b.into(),
            },
        )(i)
    }

    fn gen_into<W: Write>(&self, out: W) -> Result<(W, u64), GenError> {
        use snack::cf::*;
        cookie_factory::gen(
            bits([
                bits::u8(6, 0),
                bits::bool(self.has_control),
                bits::bool(self.function.into()),
            ]),
            out,
        )
    }
}

impl BaseFunction for Switch {}
impl BaseFunction for Bool {}
impl BaseFunction for Enable {}
impl BaseFunction for Ramp {}
impl BaseFunction for Alarm {}
impl BaseFunction for BinaryValue {}
impl BaseFunction for Step {}
impl BaseFunction for UpDown {}
impl BaseFunction for OpenClose {}
impl BaseFunction for Start {}
impl BaseFunction for State {}
impl BaseFunction for Invert {}

pub type SwitchControl = Control<Switch>;
pub type BoolControl = Control<Bool>;
pub type EnableControl = Control<Enable>;
pub type RampControl = Control<Ramp>;
pub type AlarmControl = Control<Alarm>;
pub type BinaryValueControl = Control<BinaryValue>;
pub type StepControl = Control<Step>;
pub type UpDownControl = Control<UpDown>;
pub type OpenCloseControl = Control<OpenClose>;
pub type StartControl = Control<Start>;
pub type StateControl = Control<State>;
pub type InvertControl = Control<Invert>;
