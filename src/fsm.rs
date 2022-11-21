use std::{
    collections::VecDeque,
    io::{self, ErrorKind},
    net::{Ipv4Addr, SocketAddr, SocketAddrV4, UdpSocket},
    time::{Duration, Instant},
};

use tracing::warn;

use crate::{
    connect::{ConnectRequest, Cri, KnxLayer, TunnelRequest},
    core::{Body, Frame, ServiceType},
    hpai::{HostProtocolCode, Hpai},
};

pub trait State {}

pub struct NotConnected {
    socket: UdpSocket,
}

pub struct WaitingForConnectResponse {
    waiting_since: Instant,
    endpoint: ControlEndpoint,
}

pub struct Connected {
    last_keep_alive: Instant,
    endpoint: ControlAndDataEndpoint,
}

impl State for NotConnected {}
impl State for WaitingForConnectResponse {}
impl State for Connected {}

struct ControlEndpoint {
    socket: UdpSocket,
    endpoint: SocketAddrV4,
}

fn send(socket: &UdpSocket, body: impl Into<Body>, endpoint: SocketAddrV4) -> io::Result<usize> {
    let frame = Frame::wrap(body.into());
    let buf = cookie_factory::gen_simple(frame.gen(), vec![])
        .map_err(|e| io::Error::new(ErrorKind::InvalidInput, e))?;
    socket.send_to(&buf, endpoint)
}

fn receive<F>(
    socket: &UdpSocket,
    mut timeout: Duration,
    service: ServiceType,
    allowed_endpoints: &[SocketAddrV4],
    mut unexpected_message_handler: F,
) -> io::Result<Body>
where
    F: FnMut(Body),
{
    let start = Instant::now();
    while start.elapsed() <= timeout {
        let mut buf = Vec::with_capacity(u16::MAX as usize);
        socket.set_read_timeout(Some(timeout))?;
        let (size, sender) = socket.recv_from(&mut buf[..])?;

        if size > buf.len() {
            warn!(
                "Message larger than provided buffer. Lost {} bytes of data.",
                size - buf.len()
            );
        }

        match sender {
            SocketAddr::V4(addr) if allowed_endpoints.contains(&addr) => {
                let (_, package) = Frame::parse(&buf[..size])
                    .map_err(|e| io::Error::new(ErrorKind::InvalidData, e.to_string()))?;
                if addr == allowed_endpoints[0] {
                    if package.body.as_service_type() == service {
                        return Ok(package.body);
                    } else {
                        warn!("Ignoring unexpected service message {:?}.", package.body);
                    }
                }
                unexpected_message_handler(package.body);
            }
            addr => {
                warn!(
                    "Ignoring message from wrong endpoint {addr}: {:?}",
                    &buf[..size]
                );
            }
        }

        timeout -= start.elapsed();
    }

    Err(io::Error::new(
        io::ErrorKind::TimedOut,
        "Receive timeout exceeded",
    ))
}

impl ControlEndpoint {
    fn send(&self, body: impl Into<Body>) -> io::Result<usize> {
        send(&self.socket, body, self.endpoint)
    }

    fn receive(&self, timeout: Duration, service: ServiceType) -> io::Result<Body> {
        receive(&self.socket, timeout, service, &[self.endpoint], |_| {})
    }

    fn with_data(self, data_endpoint: SocketAddrV4) -> ControlAndDataEndpoint {
        let Self { socket, endpoint } = self;
        ControlAndDataEndpoint {
            socket,
            control: endpoint,
            data: data_endpoint,
            control_buffer: Default::default(),
            data_buffer: Default::default(),
        }
    }
}

struct ControlAndDataEndpoint {
    socket: UdpSocket,
    control: SocketAddrV4,
    data: SocketAddrV4,
    control_buffer: VecDeque<Body>,
    data_buffer: VecDeque<Body>,
}

impl ControlAndDataEndpoint {
    fn send_data(&self, body: impl Into<Body>) -> io::Result<usize> {
        send(&self.socket, body, self.data)
    }

    fn send_control(&self, body: impl Into<Body>) -> io::Result<usize> {
        send(&self.socket, body, self.control)
    }

    fn receive_data(&mut self, timeout: Duration, service: ServiceType) -> io::Result<Body> {
        match self.data_buffer.pop_front() {
            Some(message) => Ok(message),
            None => receive(
                &self.socket,
                timeout,
                service,
                &[self.data, self.control],
                |message| {
                    self.control_buffer.push_back(message);
                },
            ),
        }
    }
}

pub struct Connection<T: State> {
    state: T,
}

impl Connection<NotConnected> {
    pub fn new() -> Result<Self, io::Error> {
        Ok(Self {
            state: NotConnected {
                socket: UdpSocket::bind(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 0))?,
            },
        })
    }

    pub fn connect(self, address: impl Into<SocketAddrV4>) -> KnxConnection {
        let endpoint = ControlEndpoint {
            endpoint: address.into(),
            socket: self.state.socket,
        };

        let local_endpoint = match endpoint.socket.local_addr() {
            Ok(SocketAddr::V4(ep)) => Hpai::new(HostProtocolCode::Ipv4Udp, ep),
            Ok(SocketAddr::V6(ep)) => {
                panic!("Bound UdpSocket to IpV4 addr but it reported being bound to {ep}");
            }
            Err(e) => {
                warn!("Failed to determine local addr: {e}");
                return Self {
                    state: NotConnected {
                        socket: endpoint.socket,
                    },
                }
                .into();
            }
        };
        let connect_request = ConnectRequest::new(
            local_endpoint,
            local_endpoint,
            Cri::Tunnel(TunnelRequest::new(KnxLayer::LinkLayer)),
        );
        if let Err(e) = endpoint.send(connect_request) {
            warn!("Failed to send ConnectRequest: {e}");
            return Self {
                state: NotConnected {
                    socket: endpoint.socket,
                },
            }
            .into();
        }
        Connection {
            state: WaitingForConnectResponse {
                waiting_since: Instant::now(),
                endpoint,
            },
        }
        .into()
    }
}

const CONNECT_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);

impl Connection<WaitingForConnectResponse> {
    pub fn check_response(self) -> KnxConnection {
        let response = match self
            .state
            .endpoint
            .receive(CONNECT_REQUEST_TIMEOUT, ServiceType::ConnectResponse)
        {
            Ok(Body::ConnectResponse(r)) => r,
            Ok(message) => panic!("Message of wrong type received: {message:?}"),
            Err(e) => {
                warn!("Failed to receive ConnectResponse: {e}");
                return Connection {
                    state: NotConnected {
                        socket: self.state.endpoint.socket,
                    },
                }
                .into();
            }
        };

        Connection {
            state: Connected {
                last_keep_alive: Instant::now(),
                endpoint: self
                    .state
                    .endpoint
                    .with_data(response.data_endpoint.address),
            },
        }
        .into()
    }
}

pub enum KnxConnection {
    NotConnected(Connection<NotConnected>),
    WaitingForConnectResponse(Connection<WaitingForConnectResponse>),
    Connected(Connection<Connected>),
}

impl From<Connection<NotConnected>> for KnxConnection {
    fn from(f: Connection<NotConnected>) -> Self {
        Self::NotConnected(f)
    }
}

impl From<Connection<WaitingForConnectResponse>> for KnxConnection {
    fn from(f: Connection<WaitingForConnectResponse>) -> Self {
        Self::WaitingForConnectResponse(f)
    }
}

impl From<Connection<Connected>> for KnxConnection {
    fn from(f: Connection<Connected>) -> Self {
        Self::Connected(f)
    }
}
