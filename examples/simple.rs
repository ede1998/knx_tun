use std::net::{Ipv4Addr, SocketAddrV4, UdpSocket};

use knx_tun::{
    connect::*, core::*, disconnect::DisconnectRequest, hpai::*, keep_alive::ConnectionStateRequest,
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

    fn send_frame(&self, data: impl Into<Body>, len: u16) -> std::io::Result<()> {
        let body = data.into();
        let mut frame = Frame::wrap(body);
        frame.header.body_length = len;
        let (data, len) = cookie_factory::gen(frame.gen(), vec![]).unwrap();
        self.socket.send(&data)?;
        println!("Sent data {:#?}.", data);
        Ok(())
    }

    fn receive_frame(&self) -> std::io::Result<Frame> {
        let mut buf = [0; 100];

        let (len, addr) = self
            .socket
            .recv_from(&mut buf)
            .expect("Could not receive data.");
        println!("Received {} bytes from {}.", len, addr);
        let (remainder, datagram) = Frame::parse(&buf[..len]).expect("Parsing error.");
        println!("Parsed {:#?}.", datagram);
        Ok(datagram)
    }
}

fn main() -> std::io::Result<()> {
    let socket_wrapper = SocketWrapper::try_new()?;

    let connect_request = ConnectRequest::new(
        socket_wrapper.control_ep.clone(),
        socket_wrapper.data_ep.clone(),
        Cri::new_tunnel(KnxLayer::BusMonitor),
    );
    socket_wrapper.send_frame(connect_request, 20)?;
    println!("Sent connect request.");

    let datagram = socket_wrapper.receive_frame()?;

    let connect_response = match datagram.body {
        Body::ConnectResponse(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };
    println!("Received connect response.");

    let state_request = ConnectionStateRequest::new(
        connect_response.communication_channel_id,
        socket_wrapper.control_ep.clone(),
    );

    socket_wrapper.send_frame(state_request, 4)?;
    println!("Sent connection state request.");

    let datagram = socket_wrapper.receive_frame()?;
    let state_response = match datagram.body {
        Body::ConnectionStateResponse(r) => r,
        b => panic!("Telegram of unexpected type {:#?}", b),
    };
    println!("Received connection state response.");

    let disconnect_request = DisconnectRequest::new(
        connect_response.communication_channel_id,
        socket_wrapper.control_ep.clone(),
    );
    socket_wrapper.send_frame(disconnect_request, 4)?;
    println!("Sent disconnect request.");

    let datagram = socket_wrapper.receive_frame()?;

    Ok(())
}
