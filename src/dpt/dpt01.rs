use super::general::DataPointType;
use crate::{
    cemi::GroupData,
    snack::{self, In, NomErr, U1},
};

fn parse<T>(i: In) -> Result<T, NomErr<In>>
where
    T: From<bool>,
{
    use snack::nm::*;
    map(bits(tuple((bit_u8(7), bool, eof))), |(_, b, _)| b.into())(i).map(|(_, out)| out)
}

fn to_data<T>(b: T) -> GroupData
where
    T: Into<U1>,
{
    GroupData::with_small_payload(b.into().widen())
}

macro_rules! impl_data_point_type {
    ($ty:ident, $true:ident, $false:ident, $sub_number:expr) => {
        #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
        pub enum $ty {
            $true,
            $false,
        }

        impl From<bool> for $ty {
            fn from(b: bool) -> Self {
                if b {
                    $ty::$true
                } else {
                    $ty::$false
                }
            }
        }

        impl From<$ty> for bool {
            fn from(s: $ty) -> Self {
                const FALSE: $ty = $ty::$false;
                const TRUE: $ty = $ty::$true;
                match s {
                    FALSE => false,
                    TRUE => true,
                }
            }
        }

        impl From<$ty> for U1 {
            fn from(s: $ty) -> Self {
                let s: bool = s.into();
                if s {
                    U1::_1
                } else {
                    U1::_0
                }
            }
        }

        impl DataPointType for $ty {
            const MAIN_NUMBER: u16 = 1;
            const SUB_NUMBER: u16 = $sub_number;

            type ParseError<'a> = NomErr<&'a [u8]>;
            fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>> {
                parse(group_data.get())
            }

            fn to_data(&self) -> GroupData {
                to_data(*self)
            }
        }
    };
}

impl_data_point_type!(Switch, On, Off, 1);
impl_data_point_type!(Bool, True, False, 2);
impl_data_point_type!(Enable, Enable, Disable, 3);
impl_data_point_type!(Ramp, Ramp, NoRamp, 4);
impl_data_point_type!(Alarm, Alarm, NoAlarm, 5);
impl_data_point_type!(BinaryValue, High, Low, 6);
impl_data_point_type!(Step, Increase, Decrease, 7);
impl_data_point_type!(UpDown, Down, Up, 8);
impl_data_point_type!(OpenClose, Close, Open, 9);
impl_data_point_type!(Start, Start, Stop, 10);
impl_data_point_type!(State, Active, Inactive, 11);
impl_data_point_type!(Invert, Inverted, NotInverted, 12);
impl_data_point_type!(DimSendStyle, Cyclically, StartStop, 13);
impl_data_point_type!(InputSource, Calculated, Fixed, 14);
impl_data_point_type!(Reset, ResetCommand, NoAction, 15);
impl_data_point_type!(Ack, AcknowledgeCommand, NoAction, 16);

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Trigger;

impl From<bool> for Trigger {
    fn from(_: bool) -> Self {
        Self
    }
}

impl DataPointType for Trigger {
    const MAIN_NUMBER: u16 = 1;
    const SUB_NUMBER: u16 = 17;

    type ParseError<'a> = NomErr<In<'a>>;
    fn from_data<'a>(group_data: &'a GroupData) -> Result<Self, Self::ParseError<'a>> {
        parse(group_data.get())
    }

    fn to_data(&self) -> GroupData {
        to_data(true)
    }
}

impl_data_point_type!(Occupancy, Occupied, Empty, 18);
impl_data_point_type!(WindowDoor, Open, Closed, 19);
impl_data_point_type!(LogicalFunction, And, Or, 21);
impl_data_point_type!(SceneAB, B, A, 22);
impl_data_point_type!(ShutterBlindsMode, Blind, Shutter, 23);
