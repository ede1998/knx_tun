use std::io::Write;

use cookie_factory::GenError;

use super::general::DataPointType;
use crate::snack::{self, IResult, In};

fn parse<T>(i: In) -> IResult<T>
where
    T: From<bool>,
{
    use snack::nm::*;
    map(bits(pair(bit_u8(7), bool)), |(_, b)| b.into())(i)
}

fn gen_into<T, W>(b: T, out: W) -> Result<(W, u64), GenError>
where
    T: Into<bool>,
    W: Write,
{
    use snack::cf::*;
    cookie_factory::gen(bits([bits::u8(7, 0), bits::bool(b.into())]), out)
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

        impl DataPointType for $ty {
            const MAIN_NUMBER: u16 = 1;
            const SUB_NUMBER: u16 = $sub_number;
            const LESS_THAN_A_BYTE: bool = true;

            fn parse(i: In) -> IResult<Self> {
                parse(i)
            }

            fn gen_into<W: Write>(&self, out: W) -> Result<(W, u64), GenError> {
                gen_into(*self, out)
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
    const LESS_THAN_A_BYTE: bool = true;

    fn parse(i: In) -> IResult<Self> {
        parse(i)
    }

    fn gen_into<W: Write>(&self, out: W) -> Result<(W, u64), GenError> {
        gen_into(true, out)
    }
}

impl_data_point_type!(Occupancy, Occupied, Empty, 18);
impl_data_point_type!(WindowDoor, Open, Closed, 19);
impl_data_point_type!(LogicalFunction, And, Or, 21);
impl_data_point_type!(SceneAB, B, A, 22);
impl_data_point_type!(ShutterBlindsMode, Blind, Shutter, 23);
