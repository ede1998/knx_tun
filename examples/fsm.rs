use std::net::{Ipv4Addr, SocketAddrV4};

use knx_tun::fsm::{Connection, KnxConnection};
use tracing::error;

const PORT: u16 = 3671;

fn main() -> std::io::Result<()> {
    let con = Connection::new()?;
    let KnxConnection::WaitingForConnectResponse(con) = con.connect(SocketAddrV4::new(Ipv4Addr::LOCALHOST, PORT)) else {
        error!("Transition failed");
        return Ok(());
    };
    let KnxConnection::Connected(_con) = con.check_response() else {
        error!("Transition failed");
        return Ok(());
    };
    Ok(())
}
