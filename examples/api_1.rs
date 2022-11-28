use std::{
    net::{Ipv4Addr, SocketAddrV4},
    thread::sleep,
};

use tracing::{info, metadata::LevelFilter};

use knx_tun::{
    address::{Address, RawAddress},
    api::*,
    cemi::{
        AdditionalInformation, Apdu, Cemi, CemiBody, CemiHeader, GroupData, LData, MessageCode,
        Tpdu,
    },
    snack::{U3, U5, U6},
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
    let cemi = Cemi {
        header: CemiHeader {
            message_code: MessageCode::LDataReq,
            additional_info: AdditionalInformation,
        },
        body: CemiBody::LData(LData::new(
            RawAddress::ZERO,
            Address::group(U5::_2, U3::_1, 20),
            Tpdu::DataGroup(Apdu::GroupValueWrite(GroupData::with_small_payload(U6::_1))),
        )),
    };

    tunnel_connection.send_raw(cemi)?;
    let reply = tunnel_connection.receive_raw(std::time::Duration::from_secs(2))?;
    info!("Received {reply:?}");
    tunnel_connection.keep_alive()?;
    sleep(std::time::Duration::from_secs(3));
    tunnel_connection.keep_alive()?;
    sleep(std::time::Duration::from_secs(3));
    Ok(())
}
