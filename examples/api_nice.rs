use std::{
    net::{Ipv4Addr, SocketAddrV4},
    time::Duration,
};

use tracing::{info, metadata::LevelFilter};

use knx_tun::{
    address::{Address, RawAddress},
    api_1::*,
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

    let (sender, receiver) = tunnel_connection.bidirectional(Duration::from_secs(10))?;

    for cemi in receiver {
        match cemi {
            None => {
                let cemi = Cemi {
                    header: CemiHeader {
                        message_code: MessageCode::LDataReq,
                        additional_info: AdditionalInformation,
                    },
                    body: CemiBody::LData(LData::new(
                        RawAddress::ZERO,
                        Address::group(U5::_2, U3::_1, 20),
                        Tpdu::DataGroup(Apdu::GroupValueWrite(GroupData::with_small_payload(
                            U6::_1,
                        ))),
                    )),
                };

                sender.send_raw(cemi)?;
            }
            Some(reply) => {
                info!("Received {reply:?}");
            }
        }
    }
    Ok(())
}
