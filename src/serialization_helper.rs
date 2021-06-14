use std::io::Write;
use cookie_factory::{
    bytes::{be_u16, be_u8},
    combinator::slice,
    gen,
    sequence::tuple,
    SerializeFn,
};

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