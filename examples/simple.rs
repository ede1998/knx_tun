use std::{
    any::Any,
    borrow::Borrow,
    net::{Ipv4Addr, SocketAddrV4, UdpSocket},
};

use knx_tun::{
    address::{Address, RawAddress},
    cemi::{
        AdditionalInformation, Apdu, Cemi, CemiBody, CemiHeader, GroupData, LData, MessageCode,
        Tpdu,
    },
    connect::*,
    core::*,
    disconnect::DisconnectRequest,
    hpai::*,
    keep_alive::ConnectionStateRequest,
    snack::{U3, U5, U6},
    tunneling::{ConnectionHeader, TunnelingRequest},
};

const PORT: u16 = 3671;

struct SocketWrapper {
    socket: UdpSocket,
    pub control_ep: Hpai,
    pub data_ep: Hpai,
}

impl SocketWrapper {
    fn try_new() -> std::io::Result<Self> {
        let target = SocketAddrV4::new(Ipv4Addr::LOCALHOST, PORT);
        let source = SocketAddrV4::new(Ipv4Addr::LOCALHOST, 51232);
        let socket = UdpSocket::bind(source)?;
        socket.connect(target)?;
        let control_ep = Hpai::new(HostProtocolCode::Ipv4Udp, source);
        let data_ep = Hpai::new(HostProtocolCode::Ipv4Udp, source);
        Ok(Self {
            socket,
            control_ep,
            data_ep,
        })
    }

    fn send_frame<'data, T>(&self, data: T) -> std::io::Result<()>
    where
        T: Into<Body<'data>> + Any,
    {
        let body = data.into();
        let frame = Frame::wrap(body);
        let (data, _) = cookie_factory::gen(frame.gen(), vec![]).unwrap();
        self.socket.send(&data)?;
        let name = std::any::type_name::<T>()
            .rsplit_once("::")
            .map_or("?", |r| r.1);
        println!("Sent {name}: {frame:?}");
        println!("Outgoing: {data:?}");
        Ok(())
    }

    fn receive_frame<'data>(&self, buf: &'data mut [u8]) -> std::io::Result<Frame<'data>> {
        let (len, _addr) = self.socket.recv_from(buf).expect("Could not receive data.");
        //println!("Received {len} bytes from {_addr}.");
        println!("Incoming: {:?}", &buf[..len]);
        let (_, datagram) = Frame::parse(&buf[..len]).expect("Parsing error.");
        println!(
            "Received {:?}: {datagram:?}\n\n",
            datagram.body.as_service_type()
        );
        Ok(datagram)
    }
}

fn main() -> std::io::Result<()> {
    let socket_wrapper = SocketWrapper::try_new()?;

    let connect_request = ConnectRequest::new(
        socket_wrapper.control_ep.clone(),
        socket_wrapper.data_ep.clone(),
        Cri::new_tunnel(KnxLayer::LinkLayer),
    );
    socket_wrapper.send_frame(connect_request)?;

    let mut buf = [0; 100];
    let datagram = socket_wrapper.receive_frame(&mut buf)?;

    let connect_response = match datagram.body {
        Body::ConnectResponse(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };

    socket_wrapper
        .socket
        .connect(connect_response.data_endpoint.address)?;

    let state_request = ConnectionStateRequest::new(
        connect_response.communication_channel_id,
        socket_wrapper.control_ep.clone(),
    );

    socket_wrapper.send_frame(state_request)?;

    let mut buf = [0; 100];
    let datagram = socket_wrapper.receive_frame(&mut buf)?;
    let _state_response = match datagram.body {
        Body::ConnectionStateResponse(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };

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

    let (data, _) = cookie_factory::gen(cemi.gen(), vec![]).unwrap();
    let message = TunnelingRequest {
        header: ConnectionHeader::new_empty(connect_response.communication_channel_id, 0),
        cemi: data.into(),
    };

    socket_wrapper.send_frame(message)?;

    let mut buf = [0; 100];
    let datagram = socket_wrapper.receive_frame(&mut buf)?;
    let _tunnel_ack = match datagram.body {
        Body::TunnelAck(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };

    let disconnect_request = DisconnectRequest::new(
        connect_response.communication_channel_id,
        socket_wrapper.control_ep.clone(),
    );
    socket_wrapper.send_frame(disconnect_request)?;

    let mut buf = [0; 100];
    let _datagram = socket_wrapper.receive_frame(&mut buf)?;

    Ok(())
}
