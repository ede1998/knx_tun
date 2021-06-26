#![allow(dead_code)]

pub mod address;
pub mod connect;
pub mod core;
pub mod dib;
pub mod disconnect;
pub mod hpai;
pub mod keep_alive;
pub mod snack;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
