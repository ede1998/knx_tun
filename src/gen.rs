use cookie_factory::{BackToTheBuffer, SerializeFn, bytes::{be_u16, be_u8}, combinator::{back_to_the_buffer, slice}, gen, gen_simple, sequence::tuple};
use std::io::Write;

use crate::frame::*;
use crate::header::Header;

pub(crate) fn length_be_u16<W, F>(include_self: bool, f: F) -> impl SerializeFn<W>
where
    W: Write,
    F: SerializeFn<Vec<u8>>,
{
    move |out| {
        // use a temporary buffer
        let (buf, len) = gen(&f, Vec::new())?;
        let len = if include_self { len + 2 } else { len } as u16;
        tuple((be_u16(len as u16), slice(buf)))(out)
    }
}

pub(crate) fn length_be_u8<W, F>(include_self: bool, f: F) -> impl SerializeFn<W>
where
    W: Write,
    F: SerializeFn<Vec<u8>>,
{
    move |out| {
        // use a temporary buffer
        let (buf, len) = gen(&f, Vec::new())?;
        let len = if include_self { len + 1 } else { len } as u8;
        tuple((be_u8(len), slice(buf)))(out)
    }
}

pub(crate) fn header<'a, W: Write + 'a>(m: &'a Header) -> impl SerializeFn<W> + 'a {
    length_be_u8(
        true,
        tuple((
            be_u8(m.version.into()),
            be_u16(m.service_type.into()),
            be_u16(m.body_length + Header::LENGTH),
        )),
    )
}

pub(crate) fn body<'a, W: Write + 'a>(m: &'a Body) -> impl SerializeFn<W> + 'a {
    match m {
        Body::ConnectRequest(m) => be_u16(0),
    }
}

pub(crate) fn frame<'a, W: BackToTheBuffer + 'a>(m: &'a Frame) -> impl SerializeFn<W> + 'a {
    back_to_the_buffer(Header::LENGTH as usize,
        move |buf| gen(body(&m.body), buf),
        move |buf, _| gen_simple(header(&m.header), buf))
}

#[cfg(test)]
mod tests {
    use crate::header::ServiceType;

    use super::*;

    #[test]
    fn gen_header() {
        let header = Header::new(ServiceType::DescriptionRequest, 0x1234);
        let (serialized, len) = cookie_factory::gen(super::header(&header), vec![]).unwrap();

        println!("{:#x?}", serialized);
        println!("{:#x?}", len);
        assert_eq!(serialized.len(), 0x06, "Wrong length.");
        assert_eq!(len, 0x06, "Wrong length in result.");
        assert_eq!(serialized[0], 6, "Wrong header length.");
        assert_eq!(serialized[1], 0x10, "Wrong protocol version.");
        assert_eq!(serialized[2], 0x02, "Wrong service_type high value.");
        assert_eq!(serialized[3], 0x03, "Wrong service_type low value.");
        assert_eq!(serialized[4], 0x12, "Wrong total length high value.");
        assert_eq!(serialized[5], 0x34 + 0x06, "Wrong total length high value.");
    }
}
