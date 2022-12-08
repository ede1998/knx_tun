use super::{
    dpt01::*,
    general::{DataPointId, DataPointType},
};
use crate::{
    cemi::GroupData,
    snack::{self, In, NomErr, U1},
};

pub trait BaseFunction: DataPointType + Into<U1> + From<bool> + Copy {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Control<D: BaseFunction> {
    pub has_control: bool,
    pub function: D,
}

impl<D: BaseFunction + 'static> DataPointType for Control<D> {
    const ID: DataPointId = DataPointId::new(2, D::ID.sub);

    type ParseError<'a> = NomErr<In<'a>>;

    fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>> {
        use snack::nm::*;
        map(
            bits(tuple((bit_u8(6), bool, bool, eof))),
            |(_, has_control, b, _)| Self {
                has_control,
                function: b.into(),
            },
        )(group_data.get())
        .map(|(_, out)| out)
    }

    fn to_data(&self) -> GroupData {
        let ctl: U1 = self.has_control.into();
        let function: U1 = self.function.into();
        GroupData::with_small_payload(ctl.chain(function))
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
