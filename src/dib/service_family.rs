use nom_derive::*;

use crate::snack::*;

#[derive(Copy, Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct ServiceFamily {
    pub family_id: ServiceFamilyId,
    pub version: u8,
}

impl ServiceFamily {
    pub const fn new(family_id: ServiceFamilyId, version: u8) -> Self {
        Self { family_id, version }
    }

    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        use cf::*;
        tuple((self.family_id.gen(), be_u8(self.version)))
    }

    pub(crate) fn parse(i: &[u8]) -> IResult<Self> {
        use nm::*;
        context(
            "ServiceFamily",
            map(tuple((ServiceFamilyId::parse, be_u8)), |(id, ver)| {
                Self::new(id, ver)
            }),
        )(i)
    }
}

#[derive(Copy, Debug, Clone, PartialEq, Eq, Ord, PartialOrd, NomBE)]
#[nom(GenericErrors)]
#[repr(u8)]
pub enum ServiceFamilyId {
    Core = 0x02,
    DeviceManagement = 0x03,
    Tunnelling = 0x04,
    Routing = 0x05,
    RemoteLogging = 0x06,
    RemoteConfiguration = 0x07,
    ObjectServer = 0x08,
}

impl From<ServiceFamilyId> for u8 {
    fn from(f: ServiceFamilyId) -> Self {
        f as u8
    }
}

impl ServiceFamilyId {
    pub(crate) fn gen<'a, W: Write + 'a>(&'a self) -> impl SerializeFn<W> + 'a {
        cf::be_u8((*self).into())
    }
}
