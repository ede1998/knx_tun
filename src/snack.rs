//! Re-exports of nom and cookie_factory items for ease of access through a shorthand use directive.
//! Additionally, there are helpers methods and macros here for nom and cookie factory.

pub use cookie_factory::SerializeFn;
pub use nom::IResult;
pub use std::io::Write;

/// Nom specific re-exports and helpers
pub mod nm {
    pub use nom::branch::alt;
    pub use nom::bytes::complete::{tag, take};
    pub use nom::combinator::{into, map, verify};
    pub use nom::error::ParseError;
    pub use nom::error::{make_error, ErrorKind};
    pub use nom::multi::{length_data, length_value};
    pub use nom::number::complete::*;
    pub use nom::sequence::tuple;
    pub use nom::{Err, Parser};
    pub use nom_derive::Parse;

    use nom::combinator::map_opt;
    use nom::{IResult, InputLength, InputTake, ToUsize};

    /// Wraps `length_data`. Gets a number N from the parser and returns a
    /// subslice of the input of that size. Differs from `length_data` because
    /// the size of N itself is included in the value of N.
    /// Returns `ErrorKind::MapOpt` if the parsed length is smaller than the size of
    /// the input that was parsed to determine the length.
    pub fn length_data_incl<I, N, E, F>(f: F) -> impl FnMut(I) -> IResult<I, I, E>
    where
        I: InputLength + InputTake + Clone,
        N: ToUsize,
        F: Parser<I, N, E>,
        E: ParseError<I>,
    {
        length_data(map_opt(f, |len| {
            len.to_usize().checked_sub(std::mem::size_of::<N>())
        }))
    }

    /// Wraps `length_value`. Gets a number N from the parser and returns a
    /// subslice of the input of that size. Differs from `length_data` because
    /// the size of N itself is included in the value of N.
    /// Returns `ErrorKind::MapOpt` if the parsed length is smaller than the size of
    /// the input that was parsed to determine the length.
    pub fn length_value_incl<I, O, N, E, F, G>(f: F, g: G) -> impl FnMut(I) -> IResult<I, O, E>
    where
        I: Clone + InputLength + InputTake,
        N: ToUsize,
        F: Parser<I, N, E>,
        G: Parser<I, O, E>,
        E: ParseError<I>,
    {
        length_value(
            map_opt(f, |len| {
                len.to_usize().checked_sub(std::mem::size_of::<N>())
            }),
            g,
        )
    }

    /// Reads a fixed size array with `N` items from the input.
    pub fn fixed_slice<const N: usize>(i: &[u8]) -> IResult<&[u8], [u8; N]> {
        if i.input_len() < N {
            return Err(Err::Error(make_error(i, ErrorKind::Eof)));
        }

        let mut data = [0; N];
        data.copy_from_slice(&i[..N]);
        Ok((&i[N..], data))
    }
}

/// Cookie factory specific re-exports and helpers
pub mod cf {
    pub use cookie_factory::bytes::*;
    pub use cookie_factory::combinator::{back_to_the_buffer, slice};
    pub use cookie_factory::sequence::tuple;
    pub use cookie_factory::{gen, gen_simple, WriteContext};

    use cookie_factory::{GenError, GenResult, SerializeFn};
    use std::io::Write;

    // /// Apply serializer and then write the length in front
    // #[macro_export]
    // macro_rules! length_data {
    //     ($len_type: ty, $data: expr,) => {
    //         length_data!($len_type, $data)
    //     };
    //     ($len_type: ty, $data: expr) => {
    //         back_to_the_buffer(
    //             std::mem::size_of::<$len_type>(),
    //             move |buf| gen($data, buf),
    //             move |buf, len| gen_simple(slice((len as $len_type).to_be_bytes()), buf),
    //         )
    //     };
    // }

    /// Apply serializer and then write the length in front of the serialized data.
    /// This method internally allocates a vector for buffering.
    /// `length_size` may not be larger than size of u64 (=8) or `NotYetImplemented` is returned
    /// when the serializer closure is called.
    pub fn length_data_incl<W, F>(length_size: usize, f: F) -> impl SerializeFn<W>
    where
        W: Write,
        F: SerializeFn<Vec<u8>>,
    {
        move |out| {
            let bytes_to_skip = std::mem::size_of::<u64>()
                .checked_sub(length_size)
                .ok_or(GenError::NotYetImplemented)?;

            // use a temporary buffer
            let (buf, mut len) = gen(&f, vec![])?;
            len += length_size as u64;
            let len = &len.to_be_bytes()[bytes_to_skip..];
            let result: GenResult<W> = tuple((slice(len), slice(buf)))(out);
            result
        }
    }
}

/// Create a constant named `TAG` inside a struct.
/// The macro call `make_tag!{0x05, u16}` expands to `const TAG: [u8; 2] = 0x05u16.to_be_bytes();`.
#[macro_export]
macro_rules! make_tag {
    ($value:literal, $tag_type: ty) => {
        const TAG: [u8; std::mem::size_of::<$tag_type>()] = ($value as $tag_type).to_be_bytes();
    };
}

//
///// Represents a packet that has a specific tag of the length `N`.
//pub trait Tagged<const N: usize> {
//    const TAG: [u8; N];
//}
//
///// Implements traig `Tagged` for the given type.
//#[macro_export]
//macro_rules! impl_tagged {
//    ($value:literal, $tag_type: ty, $to_tag: ty) => {
//        impl Tagged<std::mem::size_of::<$tag_type>()> for $to_tag {
//            make_tag! { $value, $tag_type }
//        }
//    };
//}
