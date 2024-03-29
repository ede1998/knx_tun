use num_enum::{IntoPrimitive, TryFromPrimitive};

use super::{
    dpt01::{Step, UpDown},
    general::{DataPointId, DataPointType},
};
use crate::{
    cemi::GroupData,
    snack::{self, GenError, In, NomErr, U1, U3},
};

fn parse<T, F, A>(i: In, mut mapper: F) -> Result<T, NomErr<In>>
where
    F: FnMut(A, StepCode) -> T,
    A: From<bool>,
{
    use snack::nm::*;
    map(bits(tuple((bit_u8(4), bool, U3::parse))), |(_, b, u3)| {
        mapper(b.into(), u3.into())
    })(i)
    .map(|(_, out)| out)
}

fn gen_into<T, W>(b: T, step_code: StepCode, out: W) -> Result<(W, u64), GenError>
where
    T: Into<bool>,
    W: std::io::Write,
{
    use snack::cf::*;
    cookie_factory::gen(
        bits([
            bits::u8(4, 0),
            bits::bool(b.into()),
            U3::from(step_code).into(),
        ]),
        out,
    )
}

#[derive(
    Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash, TryFromPrimitive, IntoPrimitive,
)]
#[repr(u8)]
pub enum StepCode {
    Break = 0,
    _1 = 1,
    _2 = 2,
    _4 = 3,
    _8 = 4,
    _16 = 5,
    _32 = 6,
    _64 = 7,
}

impl StepCode {
    pub fn intervals(&self) -> u8 {
        match self {
            StepCode::Break => 0,
            StepCode::_1 => 1,
            StepCode::_2 => 2,
            StepCode::_4 => 4,
            StepCode::_8 => 8,
            StepCode::_16 => 16,
            StepCode::_32 => 32,
            StepCode::_64 => 64,
        }
    }

    pub fn try_from_interval(interval: u8) -> Result<Self, u8> {
        match interval {
            0 => Ok(Self::Break),
            1 => Ok(Self::_1),
            2 => Ok(Self::_2),
            4 => Ok(Self::_4),
            8 => Ok(Self::_8),
            16 => Ok(Self::_16),
            32 => Ok(Self::_32),
            64 => Ok(Self::_64),
            i => Err(i),
        }
    }
}

impl From<U3> for StepCode {
    fn from(d: U3) -> Self {
        Self::try_from(d).expect("U3 only represents 3 bits so the conversion is infalliable.")
    }
}

impl From<StepCode> for U3 {
    fn from(sc: StepCode) -> Self {
        U3::new(sc.into())
            .expect("StepCode contains exactly 8 variants with discriminants in range 0..=7 so the conversion is infalliable.")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ControlDimming {
    pub action: Step,
    pub step_code: StepCode,
}

impl ControlDimming {
    pub const fn new(action: Step, step_code: StepCode) -> Self {
        Self { action, step_code }
    }
}

impl DataPointType for ControlDimming {
    const ID: DataPointId = DataPointId::new(3, Step::ID.sub);

    type ParseError<'a> = NomErr<In<'a>>;

    fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>> {
        parse(group_data.get(), Self::new)
    }

    fn to_data(&self) -> GroupData {
        let step: U1 = self.action.into();
        let step_code = self.step_code.into();
        GroupData::with_small_payload(step.chain::<3, 4>(step_code).widen())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ControlBlinds {
    pub action: UpDown,
    pub step_code: StepCode,
}

impl ControlBlinds {
    pub const fn new(action: UpDown, step_code: StepCode) -> Self {
        Self { action, step_code }
    }
}

impl DataPointType for ControlBlinds {
    const ID: DataPointId = DataPointId::new(3, UpDown::ID.sub);

    type ParseError<'a> = NomErr<In<'a>>;

    fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>> {
        parse(group_data.get(), Self::new)
    }

    fn to_data(&self) -> GroupData {
        let step: U1 = self.action.into();
        let step_code = self.step_code.into();
        GroupData::with_small_payload(step.chain::<3, 4>(step_code).widen())
    }
}
