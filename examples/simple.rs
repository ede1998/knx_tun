use std::net::{Ipv4Addr, SocketAddrV4, UdpSocket};

use knx_tun::{
    address::{Address, AddressKind},
    cemi::{AdditionalInformation, Apdu, Cemi, CemiBody, CemiHeader, LData, MessageCode, Tpdu, GroupData},
    connect::*,
    core::*,
    disconnect::DisconnectRequest,
    hpai::*,
    keep_alive::ConnectionStateRequest,
    tunneling::{ConnectionHeader, TunnelingRequest}, snack::U6,
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

    fn send_frame<'data>(&self, data: impl Into<Body<'data>>) -> std::io::Result<()> {
        let body = data.into();
        let frame = Frame::wrap(body);
        let (data, _) = cookie_factory::gen(frame.gen(), vec![]).unwrap();
        self.socket.send(&data)?;
        println!("Sent data {:#?}.", data);
        Ok(())
    }

    fn receive_frame<'data>(&self, buf: &'data mut [u8]) -> std::io::Result<Frame<'data>> {
        let (len, addr) = self
            .socket
            .recv_from(buf)
            .expect("Could not receive data.");
        println!("Received {len} bytes from {addr}.");
        let (_, datagram) = Frame::parse(&buf[..len]).expect("Parsing error.");
        println!("Parsed {:#?}.", datagram);
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
    println!("Sent connect request.");

    let mut buf = [0; 100];
    let datagram = socket_wrapper.receive_frame(&mut buf)?;

    let connect_response = match datagram.body {
        Body::ConnectResponse(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };
    println!("Received connect response.");

    let state_request = ConnectionStateRequest::new(
        connect_response.communication_channel_id,
        socket_wrapper.control_ep.clone(),
    );

    socket_wrapper.send_frame(state_request)?;
    println!("Sent connection state request.");

    let mut buf = [0; 100];
    let datagram = socket_wrapper.receive_frame(&mut buf)?;
    let state_response = match datagram.body {
        Body::ConnectionStateResponse(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };
    println!("Received connection state response {:#?}.", state_response);

    let cemi = Cemi {
        header: CemiHeader {
            message_code: MessageCode::LDataReq,
            additional_info: AdditionalInformation,
        },
        body: CemiBody::LData(LData::new(
            Address::zero(AddressKind::Individual),
            Address::new(AddressKind::Group, 0x21, 20),
            Tpdu::DataGroup(Apdu::GroupValueWrite(GroupData::with_small_payload(U6::_1))),
        )),
    };

    let (data, _) = cookie_factory::gen(cemi.gen(), vec![]).unwrap();
    let message = TunnelingRequest {
        header: ConnectionHeader::new_empty(connect_response.communication_channel_id, 0),
        cemi: data.into(),
    };

    socket_wrapper.send_frame(message)?;

    let disconnect_request = DisconnectRequest::new(
        connect_response.communication_channel_id,
        socket_wrapper.control_ep.clone(),
    );
    socket_wrapper.send_frame(disconnect_request)?;
    println!("Sent disconnect request.");

    let mut buf = [0; 100];
    let datagram = socket_wrapper.receive_frame(&mut buf)?;

    println!("Received datagram {:#?}.", datagram);

    Ok(())
}
