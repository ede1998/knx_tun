//! Re-exports of nom and cookie_factory items for ease of access through a shorthand use directive.
//! Additionally, there are helpers methods and macros here for nom and cookie factory.

pub use cookie_factory::SerializeFn;
pub use std::io::Write;
use std::ops::Deref;

pub type IResult<'a, O> = nom::IResult<In<'a>, O, nm::Error<In<'a>>>;
pub type IBitResult<'a, O> = nom::IResult<(In<'a>, usize), O, nm::Error<(In<'a>, usize)>>;
pub type In<'a> = &'a [u8];

/// Nom specific re-exports and helpers
pub mod nm {
    pub use nom::bits::complete::{tag as bit_tag, take as bit_take};
    pub use nom::branch::alt;
    pub use nom::bytes::complete::{tag, take};
    pub use nom::combinator::{all_consuming, eof, into, map, map_res, rest, verify};
    pub use nom::error::{
        context, make_error, ContextError as NomContextError, Error as NomError,
        ErrorKind as NomErrorKind, ParseError as NomParseError, VerboseError,
    };
    pub use nom::multi::{length_data, length_value, many0};
    pub use nom::number::complete::*;
    pub use nom::sequence::{pair, preceded, tuple};
    pub use nom::{Err, Parser};
    pub use nom_derive::Parse;

    use super::In;
    use nom::combinator::map_opt;
    use nom::error::FromExternalError;
    use nom::{ErrorConvert, IResult, InputLength, InputTake, ToUsize};
    use std::fmt;

    #[derive(Debug)]
    pub enum ErrorKind {
        Nom(NomErrorKind),
        Context(&'static str),
    }

    pub struct Error<I> {
        pub errors: Vec<(I, ErrorKind)>,
    }

    impl<I> NomParseError<I> for Error<I> {
        fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
            let errors = vec![(input, ErrorKind::Nom(kind))];
            Self { errors }
        }

        fn append(input: I, kind: NomErrorKind, mut other: Self) -> Self {
            other.errors.push((input, ErrorKind::Nom(kind)));
            other
        }
    }

    impl<I> NomContextError<I> for Error<I> {
        fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
            other.errors.push((input, ErrorKind::Context(ctx)));
            other
        }
    }

    impl<'a> fmt::Debug for Error<In<'a>> {
        /// Algorithm copied from https://fasterthanli.me/series/making-our-own-ping/part-9
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            writeln!(f, "/!\\ parsing error")?;

            let mut shown_input = None;
            let margin_left = 4;
            let margin_str = " ".repeat(margin_left);

            // maximum amount of binary data we'll dump per line
            let maxlen = 60;

            // given a big slice, an offset, and a length, attempt to show
            // some data before, some data after, and highlight which part
            // we're talking about with tildes.
            let print_slice =
                |f: &mut fmt::Formatter, s: In, offset: usize, len: usize| -> fmt::Result {
                    // decide which part of `s` we're going to show.
                    let (s, offset, len) = {
                        // see diagram further in article.
                        // TODO: review for off-by-one errors

                        let avail_after = s.len() - offset;
                        let after = std::cmp::min(avail_after, maxlen / 2);

                        let avail_before = offset;
                        let before = std::cmp::min(avail_before, maxlen / 2);

                        let new_start = offset - before;
                        let new_end = offset + after;
                        let new_offset = before;
                        let new_len = std::cmp::min(new_end - new_start, len);

                        (&s[new_start..new_end], new_offset, new_len)
                    };

                    write!(f, "{}", margin_str)?;
                    for b in s {
                        write!(f, "{:02X} ", b)?;
                    }
                    writeln!(f)?;

                    write!(f, "{}", margin_str)?;
                    for i in 0..s.len() {
                        // each byte takes three characters, ie "FF "
                        if i == offset + len - 1 {
                            // ..except the last one
                            write!(f, "~~")?;
                        } else if (offset..offset + len).contains(&i) {
                            write!(f, "~~~")?;
                        } else {
                            write!(f, "   ")?;
                        };
                    }
                    writeln!(f)?;

                    Ok(())
                };

            for (input, kind) in self.errors.iter().rev() {
                let prefix = match kind {
                    ErrorKind::Context(ctx) => format!("...in {}", ctx),
                    ErrorKind::Nom(err) => format!("nom error {:?}", err),
                };

                writeln!(f, "{}", prefix)?;
                match shown_input {
                    None => {
                        shown_input.replace(input);
                        print_slice(f, input, 0, input.len())?;
                    }
                    Some(parent_input) => {
                        // `nom::Offset` is a trait that lets us get the position
                        // of a subslice into its parent slice. This works great for
                        // our error reporting!
                        use nom::Offset;
                        let offset = parent_input.offset(input);
                        print_slice(f, parent_input, offset, input.len())?;
                    }
                };
            }
            Ok(())
        }
    }

    impl<'a> ErrorConvert<Error<(In<'a>, usize)>> for Error<In<'a>> {
        fn convert(self) -> Error<(In<'a>, usize)> {
            Error {
                errors: self
                    .errors
                    .into_iter()
                    .map(|(pos, kind)| ((pos, 0), kind))
                    .collect(),
            }
        }
    }

    impl<'a> ErrorConvert<Error<In<'a>>> for Error<(In<'a>, usize)> {
        fn convert(self) -> Error<In<'a>> {
            Error {
                errors: self
                    .errors
                    .into_iter()
                    .map(|(pos, kind)| (pos.0, kind))
                    .collect(),
            }
        }
    }

    impl<'a, I> FromExternalError<I, ()> for Error<I> {
        fn from_external_error(input: I, kind: NomErrorKind, _: ()) -> Self {
            Self::from_error_kind(input, kind)
        }
    }

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
        E: NomParseError<I>,
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
        E: NomParseError<I>,
    {
        length_value(
            map_opt(f, |len| {
                len.to_usize().checked_sub(std::mem::size_of::<N>())
            }),
            g,
        )
    }

    pub fn length_value_offset<I, O, N, E, F, G>(
        f: F,
        offset: N,
        g: G,
    ) -> impl FnMut(I) -> IResult<I, O, E>
    where
        I: Clone + InputLength + InputTake,
        N: ToUsize,
        F: Parser<I, N, E>,
        G: Parser<I, O, E>,
        E: NomParseError<I>,
    {
        length_value(
            map_opt(f, move |len| len.to_usize().checked_add(offset.to_usize())),
            g,
        )
    }
    /// Repeats the embedded parser until the input is exhausted
    /// and returns the results in a `Vec`.
    ///
    /// If the embedded parser returns an error, `all0` stops parsing
    /// and returns this error.
    ///
    /// # Arguments
    /// * `f` The parser to apply.
    ///
    /// *Note*: if the parser passed to `all0` accepts empty inputs
    /// (like `alpha0` or `digit0`), `all0` will return an error,
    /// to prevent going into an infinite loop
    ///
    /// ```rust
    /// # use nom::{Err, error::{ErrorKind, Error}, Needed, IResult};
    /// use knx_tun::snack::nm::all0;
    /// use nom::bytes::complete::tag;
    ///
    /// fn parser(s: &str) -> IResult<&str, Vec<&str>> {
    ///   all0(tag("abc"))(s)
    /// }
    ///
    /// assert_eq!(parser("abcabc"), Ok(("", vec!["abc", "abc"])));
    /// assert_eq!(parser("abc123"), Err(Err::Error(Error::new("123", ErrorKind::Tag))));
    /// assert_eq!(parser("123123"), Err(Err::Error(Error::new("123123", ErrorKind::Tag))));
    /// assert_eq!(parser(""), Ok(("", vec![])));
    /// ```
    pub fn all0<I, O, E, F>(mut f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
    where
        I: InputLength,
        F: Parser<I, O, E>,
        E: NomParseError<I>,
    {
        move |mut i: I| {
            let mut acc = Vec::with_capacity(4);
            let mut old_len = i.input_len();
            while old_len > 0 {
                match f.parse(i) {
                    Ok((inner, o)) => {
                        let new_len = inner.input_len();
                        if new_len == old_len {
                            return Err(Err::Error(E::from_error_kind(inner, NomErrorKind::Many0)));
                        }
                        acc.push(o);
                        i = inner;
                        old_len = new_len;
                    }
                    Err(e) => return Err(e),
                }
            }
            Ok((i, acc))
        }
    }

    /// Reads a fixed size array with `N` items from the input.
    pub fn fixed_slice<const N: usize>(i: In) -> super::IResult<[u8; N]> {
        if i.input_len() < N {
            return Err(Err::Error(make_error(i, NomErrorKind::Eof)));
        }

        let mut data = [0; N];
        data.copy_from_slice(&i[..N]);
        Ok((&i[N..], data))
    }

    pub fn bool<'a, E>(i: (In<'a>, usize)) -> IResult<(In<'a>, usize), bool, E>
    where
        E: NomParseError<(In<'a>, usize)>,
    {
        map(bit_take(1usize), |b: u8| b == 1)(i)
    }

    pub fn bit_u8<'a, E>(
        count: u8,
    ) -> impl FnMut((In<'a>, usize)) -> IResult<(In<'a>, usize), u8, E>
    where
        E: NomParseError<(In<'a>, usize)>,
    {
        bit_take(count)
    }

    pub fn bits<'a, O, P>(parser: P) -> impl FnMut(In<'a>) -> IResult<In<'a>, O, Error<In<'a>>>
    where
        P: FnMut((In<'a>, usize)) -> IResult<(In<'a>, usize), O, Error<(In<'a>, usize)>>,
    {
        nom::bits::bits(parser)
    }

    pub fn bytes<'a, O, P>(
        parser: P,
    ) -> impl FnMut((In<'a>, usize)) -> IResult<(In<'a>, usize), O, Error<(In<'a>, usize)>>
    where
        P: FnMut(In<'a>) -> IResult<In<'a>, O, Error<In<'a>>>,
    {
        nom::bits::bytes(parser)
    }
}

/// Cookie factory specific re-exports and helpers
pub mod cf {
    pub use cookie_factory::bytes::*;
    pub use cookie_factory::combinator::{back_to_the_buffer, slice};
    pub use cookie_factory::multi::all;
    pub use cookie_factory::sequence::{pair, tuple};
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

    /// Serializes a stream of single bits.
    /// If the bit stream ends mid-byte, the remainder of the last bytes is filled
    /// with 0.
    pub fn bits<W, C>(collection: C) -> impl SerializeFn<W>
    where
        W: Write,
        C: IntoIterator<Item = Bits> + Clone,
    {
        fn remove_excess(value: u64, start: u8, count: u8) -> u64 {
            let excess_bits_right_removed = value >> (start - count);
            let keep_count_bits = (1 << count) - 1;
            excess_bits_right_removed & keep_count_bits
        }

        move |mut w| {
            let mut remaining_output_bits = 8;
            let mut data = 0;
            for b in collection.clone() {
                let mut remaining_input_bits = b.len;
                while remaining_input_bits > 0 {
                    let bit_count = remaining_input_bits.min(remaining_output_bits);
                    let bits = remove_excess(b.value, remaining_input_bits, bit_count);
                    let aligned_bits = bits << (remaining_output_bits - bit_count);

                    data |= aligned_bits as u8;
                    remaining_output_bits = if remaining_output_bits > bit_count {
                        remaining_output_bits - bit_count
                    } else {
                        w = be_u8(data)(w)?;
                        data = 0;
                        8
                    };
                    remaining_input_bits -= bit_count;
                }
            }
            if remaining_output_bits != 8 {
                be_u8(data)(w)
            } else {
                Ok(w)
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct Bits {
        value: u64,
        len: u8,
    }

    impl Bits {
        pub fn new(len: u8, value: u64) -> Self {
            Self { value, len }
        }
    }

    pub mod bits {
        use super::Bits;
        pub fn u8(len: u8, value: u8) -> Bits {
            Bits::new(len, value.into())
        }

        pub fn u16(len: u8, value: u16) -> Bits {
            Bits::new(len, value.into())
        }

        pub fn bool(b: bool) -> Bits {
            Bits::new(1, b.into())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn bits_single() {
            use bits::{bool, u8};
            let mut buf = [0; 5];
            {
                let (new_buf, pos) = cookie_factory::gen(
                    bits([u8(1, 1), bool(false), u8(2, 0b10), u8(4, 0b1110)]),
                    &mut buf[..],
                )
                .unwrap();

                assert_eq!([0; 4], new_buf);
                assert_eq!(1, pos);
            }

            assert_eq!([0b10101110, 0, 0, 0, 0], buf);
        }

        #[test]
        #[allow(clippy::unusual_byte_groupings)] // used to demark bit groups
        fn bits_many() {
            use bits::{bool, u16, u8};
            let mut buf = [0; 5];
            {
                let (new_buf, pos) = cookie_factory::gen(
                    bits([
                        u8(2, 0b11),
                        u16(11, 0b11100011110),
                        bool(true),
                        u8(4, 0b0011),
                    ]),
                    &mut buf[..],
                )
                .unwrap();

                assert_eq!([0; 2], new_buf);
                assert_eq!(3, pos);
            }

            assert_eq!([0b11_111000, 0b11110_1_00, 0b11_000000, 0, 0], buf);
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

pub struct OutOfRangeError;

pub type U1 = U<1>;
pub type U2 = U<2>;
pub type U3 = U<3>;
pub type U4 = U<4>;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct U<const N: u8>(u8);

impl<const N: u8> U<N> {
    pub const MAX_VALUE: u8 = (1 << N) - 1;

    pub const fn new(data: u8) -> Result<Self, OutOfRangeError> {
        if data <= Self::MAX_VALUE {
            Ok(Self(data))
        } else {
            Err(OutOfRangeError)
        }
    }

    pub const fn unwrap(data: u8) -> Self {
        match Self::new(data) {
            Ok(u) => u,
            Err(_) => panic!("Data was out of bounds."),
        }
    }

    pub(crate) fn parse(i: (In, usize)) -> IBitResult<Self> {
        use nm::*;
        map(bit_u8(N), Self)(i)
    }
}

impl<const N: u8> Deref for U<N> {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const N: u8> From<U<N>> for cf::Bits {
    fn from(u: U<N>) -> Self {
        cf::bits::u8(N as u8, u.0)
    }
}

impl<const N: u8> TryFrom<u8> for U<N> {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Self::new(value).map_err(|_| value)
    }
}
