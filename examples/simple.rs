use std::net::{Ipv4Addr, SocketAddrV4, UdpSocket};

use knx_tun::{connect::*, core::*, hpai::*};

const PORT: u16 = 3671;

fn main() -> std::io::Result<()> {
    println!("Test");
    let target = SocketAddrV4::new(Ipv4Addr::LOCALHOST, PORT);
    let source = SocketAddrV4::new(Ipv4Addr::LOCALHOST, 51232);
    let socket = UdpSocket::bind(source)?;
    socket.connect(target)?;

    let connect_request = ConnectRequest::new(
        Hpai::new(HostProtocolCode::Ipv4Udp, source),
        Hpai::new(HostProtocolCode::Ipv4Udp, source),
        Cri::new_tunnel(KnxLayer::BusMonitor),
    );
    let body = connect_request.into();
    let mut frame = Frame::wrap(body);
    frame.header.body_length = 20;

    let (data, len) = cookie_factory::gen(frame.gen(), vec![]).unwrap();
    socket.send(&data)?;

    let mut buf = [0; 100];

    let (len, addr) = socket.recv_from(&mut buf).expect("Could not receive data.");
    println!("Received {} bytes from {}.", len, addr);
    let (remainder, datagram) = Frame::parse(&buf[..len]).expect("Parsing error.");
    println!("Parsed {:#?}.", datagram);

    Ok(())
}
