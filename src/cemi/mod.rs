use cookie_factory::BackToTheBuffer;
use nom_derive::NomBE;

use crate::snack::*;
pub use ldata::*;
pub use npdu::*;

mod ldata;
mod npdu;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum MessageCode {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Cemi {
    pub header: CemiHeader,
    pub body: CemiBody,
}

impl Cemi {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        let (i, header) = CemiHeader::parse(i)?;
        let (i, body) = match header.message_code {
            MessageCode::LDataCon | MessageCode::LDataInd | MessageCode::LDataReq => {
                into(LData::parse)(i)?
            }
            _ => unimplemented!(),
        };
        Ok((i, Self { header, body }))
    }

    pub fn gen<'a, W: BackToTheBuffer + Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        let CemiBody::LData(body) = &self.body;
        pair(self.header.gen(), body.gen())
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct CemiHeader {
    pub message_code: MessageCode,
    pub additional_info: AdditionalInformation,
}

impl CemiHeader {
    pub(crate) fn parse(i: In) -> IResult<Self> {
        use nm::*;
        context(
            stringify!(CemiHeader),
            map(
                pair(MessageCode::parse, AdditionalInformation::parse),
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum CemiBody {
    LData(LData),
}

impl From<LData> for CemiBody {
    fn from(f: LData) -> Self {
        Self::LData(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AdditionalInformation;

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::address::{Address, AddressKind, RawAddress};
    use npdu::{Apdu, GroupData, Tpdu};

    #[rustfmt::skip]
    const CEMI_IND_GROUP_VALUE_WRITE: [u8; 11] = [
        0x29, // message code L_DATA.ind
        0x00, // Additional info len = 0 bytes
        0xbc, // CTRL1
        0xe0, // CTRL 2
        0x11, 0x0b, // source device address
        0x69, 0x01, // target address (group)
        0x01, // length 1
        0b000000_00, // TPCI: T_Data_Group-PDU (destination_address <> 0), APCI: first 2 bits of tag
        #[allow(clippy::unusual_byte_groupings)] // grouped by related bits
        0b10_000001, // APCI: A_GroupValue_Write-PDU, data: 1
    ];

    const STRUCT_CEMI_IND_GROUP_VALUE_WRITE: Cemi = Cemi {
        header: CemiHeader {
            message_code: MessageCode::LDataInd,
            additional_info: AdditionalInformation,
        },
        body: CemiBody::LData(LData {
            control_1: Control1 {
                is_standard_frame: true,
                do_not_repeat: true,
                is_normal_broadcast: true,
                priority: Priority::Low,
                acknowledge_requested: false,
                had_transmission_error: false,
            },
            control_2: Control2 {
                destination_address: AddressKind::Group,
                hop_count: U3::_6,
                frame_format: FrameFormat::Standard,
            },
            source: Address::new(AddressKind::Individual, 0x11, 0x0b),
            destination: RawAddress {
                subnet: 0x69,
                device: 0x01,
            },
            npdu: Npdu(Tpdu::DataGroup(Apdu::GroupValueWrite(
                GroupData::with_small_payload(U6::_1),
            ))),
        }),
    };

    #[test]
    fn parse_cemi() {
        let (rem, actual) = Cemi::parse(&CEMI_IND_GROUP_VALUE_WRITE).unwrap();

        assert_eq!(0, rem.len());
        assert_eq!(actual, STRUCT_CEMI_IND_GROUP_VALUE_WRITE);
    }

    #[test]
    fn parse_cemi_panicked() {
        let bytes = [
            0x2e, 0x00, 0xb4, 0xe0, 0x71, 0x0a, 0x11, 0x14, 0x02, 0x00, 0x80, 0x01,
        ];
        let (i, cemi) = Cemi::parse(&bytes).unwrap();
        assert!(i.is_empty());
        assert_eq!(cemi.header.message_code, MessageCode::LDataCon);
    }
}
