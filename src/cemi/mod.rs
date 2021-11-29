use crate::{
    address::{Address, AddressKind},
    snack::*,
};
use nom_derive::NomBE;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
enum MessageCode {
    LBusmonInd = 0x2B,
    LDataCon = 0x2E,
    LDataInd = 0x29,
    LDataReq = 0x11,
    LPollDataCon = 0x25,
    LPollDataReq = 0x13,
    LRawCon = 0x2F,
    LRawInd = 0x2D,
    LRawReq = 0x10,
    /// MFuncPropStateread.con
    MFuncPropCommandCon = 0xFA,
    MFuncPropCommandReq = 0xF8,
    MFuncPropStateReadReq = 0xF9,
    MPropInfoInd = 0xF7,
    MPropReadCon = 0xFB,
    MPropReadReq = 0xFC,
    MPropWriteCon = 0xF5,
    MPropWriteReq = 0xF6,
    MResetInd = 0xF0,
    MResetReq = 0xF1,
    TDataConnectedInd = 0x89,
    TDataConnectedReq = 0x41,
    TDataIndividualInd = 0x94,
    TDataIndividualReq = 0x4A,
}

impl From<MessageCode> for u8 {
    fn from(f: MessageCode) -> Self {
        f as _
    }
}

struct CemiHeader {
    pub message_code: MessageCode,
    pub additional_info: AdditionalInformation,
}

impl CemiHeader {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(CemiHeader),
            map(
                tuple((MessageCode::parse, AdditionalInformation::parse)),
                |(mc, ai)| Self {
                    message_code: mc,
                    additional_info: ai,
                },
            ),
        )(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((be_u8(self.message_code.into()), self.additional_info.gen()))
    }
}

enum CemiBody {
    LData(LData),
}

struct LData {
    control_1: u8,
    control_2: u8,
    source: Address,
    destination: Address,
}

impl LData {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        let (i, (control_1, control_2)) = tuple((be_u8, be_u8))(i)?;
        let (i, source) = Address::parse(i, AddressKind::Individual)?;
        let dest_addr_kind = if (control_2 >> 7) == 1 {
            AddressKind::Group
        } else {
            AddressKind::Individual
        };
        let (i, destination) = Address::parse(i, dest_addr_kind)?;
        Ok((
            i,
            Self {
                control_1,
                control_2,
                source,
                destination,
            },
        ))
    }

    /// Sensible default for CTRL_1 for now.
    #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
    const CONTROL_1: u8 = 0b1_0_1_1_00_0_0;
    /// Sensible default for CTRL_2 for now.
    #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
    const CONTROL_2: u8 = 0b1_111_0000;

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        let control2 = match self.destination.kind() {
            AddressKind::Group => self.control_2 | 0b1000_0000,
            AddressKind::Individual => self.control_2 & !0b1000_0000,
        };
        tuple((
            be_u8(self.control_1),
            be_u8(control2),
            self.source.gen(),
            self.destination.gen(),
        ))
    }
}

struct AdditionalInformation;

impl AdditionalInformation {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        map(length_data(be_u8), |_| Self)(i)
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        be_u8(0)
    }
}
