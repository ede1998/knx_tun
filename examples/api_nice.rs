use std::{
    net::{Ipv4Addr, SocketAddrV4},
    time::Duration,
};

use tracing::{info, metadata::LevelFilter};

use knx_tun::{
    address::GroupAddress,
    api::*,
    dpt::dpt01::Switch,
    snack::{U3, U5},
};

const PORT: u16 = 3671;

fn main() -> Result<(), ConnectionError> {
    tracing::subscriber::set_global_default(
        tracing_subscriber::fmt()
            .with_max_level(LevelFilter::TRACE)
            .finish(),
    )
    .expect("Failed to initialize logger");
    let mut tunnel_connection = TunnelConnection::new()?;
    tunnel_connection.open(SocketAddrV4::new(Ipv4Addr::LOCALHOST, PORT))?;

    let (sender, receiver) = tunnel_connection
        .bidirectional::<GroupMessage<Switch>>(Duration::from_secs(10))
        .map_err(|(_, e)| e)?;

    for cemi in receiver {
        match cemi {
            None => {
                sender.send(Switch::On, GroupAddress::new(U5::_2, U3::_1, 20))?;
            }
            Some(reply) => {
                info!("Received {reply:?}");
            }
        }
    }
    Ok(())
}
