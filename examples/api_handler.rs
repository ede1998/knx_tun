use std::{
    net::{Ipv4Addr, SocketAddrV4},
    time::Duration,
};

use tracing::{info, metadata::LevelFilter, warn};

use knx_tun::{
    address::GroupAddress,
    api::*,
    dpt::dpt01::Switch,
    snack::{U3, U5},
};

const PORT: u16 = 3671;
const GROUP: GroupAddress = GroupAddress::new(U5::_2, U3::_1, 20);

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
        .bidirectional::<knx_tun::cemi::Cemi>(Duration::from_secs(10))
        .map_err(|(_, e)| e)?;

    let mut first_time = true;
    let mut handlers = Handlers::default();
    handlers.register(GROUP, |message: GroupMessage<Switch>| {
        info!("Received {message:?}");
        if first_time {
            first_time = false;
            sender.send(Switch::Off, message.address).is_ok()
        } else {
            true
        }
    });

    for cemi in receiver {
        match cemi {
            None => {
                sender.send(Switch::On, GROUP)?;
            }
            Some(reply) => {
                if let Err(e) = handlers.handle_raw(&reply) {
                    warn!("Error {e} while handling {reply:?}.");
                }
            }
        }
    }
    Ok(())
}
