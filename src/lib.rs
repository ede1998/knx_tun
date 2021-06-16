
pub mod hpai;
pub mod core;
pub mod cri;
pub mod dib;
pub mod address;
mod gen;
mod parse;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
