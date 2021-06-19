use std::net::{Ipv4Addr, SocketAddrV4, UdpSocket};

fn main(){}
// use knx_tun::{*, cri::{ConnectRequest, ConnectionRequestInformation, KnxLayer}, frame::{Body, Frame}, header::{Header, ServiceType}, hpai::HostProtocolAddressInformation};

// const PORT: u16 = 3671;

// fn main() -> std::io::Result<()> {
//     println!("Test");
//     let target = SocketAddrV4::new(Ipv4Addr::LOCALHOST, PORT);
//     let source = SocketAddrV4::new(Ipv4Addr::LOCALHOST, 0);
//     let socket = UdpSocket::bind(source)?;
//     socket.connect(target)?;

//     let mut header = Header::new(ServiceType::ConnectRequest,0);

//         control_endpoint: HostProtocolAddressInformation::new(hpai::HostProtocolCode::Ipv4Udp, source),
//         data_endpoint: HostProtocolAddressInformation::new(hpai::HostProtocolCode::Ipv4Udp, source),
//         cri: ConnectionRequestInformation::Tunnel(KnxLayer::BusMonitor),
//     };
//     let body = Body::ConnectRequest(request);
//     // header.set_body_len(body.len());
//     let frame = Frame::new(header, body);

//     let data = frame.serialize();
//     socket.send(&data)?;

//     Ok(())
// }