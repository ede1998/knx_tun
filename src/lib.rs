
pub mod header;
pub mod hpai;
pub mod frame;
pub mod cri;
pub mod dib;
pub mod address;
mod serialization_helper;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
