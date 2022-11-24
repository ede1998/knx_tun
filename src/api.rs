use std::{
    collections::VecDeque,
    io,
    net::{Ipv4Addr, SocketAddr, SocketAddrV4, UdpSocket},
    time::{Duration, Instant},
};

use thiserror::Error;
use tracing::{debug, trace, warn};

use crate::{
    cemi::Cemi,
    connect::{ConnectRequest, Cri, KnxLayer, TunnelRequest},
    core::{Body, Frame, ServiceType},
    disconnect::{DisconnectRequest, DisconnectState},
    hpai::{HostProtocolCode, Hpai},
    keep_alive::{ConnectionState, ConnectionStateRequest},
    snack::{In, NomErr},
    tunneling::{TunnelingAck, TunnelingAckState, TunnelingRequest},
};

#[derive(Debug, Error)]
pub enum ConnectionError {
    #[error("network error: {0}")]
    IoError(#[from] io::Error),
    #[error("could not serialize packet: {0}")]
    GenError(#[from] cookie_factory::GenError),
    #[error("could not parse packet: {0:?}")]
    ParseError(#[from] NomErr<Vec<u8>>),
}

impl<'a> From<NomErr<In<'a>>> for ConnectionError {
    fn from(f: NomErr<In<'a>>) -> Self {
        Self::ParseError(NomErr::<Vec<u8>>::convert(f))
    }
}

pub struct TunnelConnection {
    socket: Socket,
    channel_id: u8,
    last_keep_alive: Instant,
}

const CONNECT_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);
const CONNECTION_STATE_REQUEST_INTERVAL: Duration = Duration::from_secs(60);
const CONNECTION_STATE_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);
const DISCONNECT_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);
const TUNNELING_REQUEST_TIMEOUT: Duration = Duration::from_secs(1);

impl TunnelConnection {
    pub fn open(remote: SocketAddrV4) -> Result<Self, ConnectionError> {
        let mut socket = UnconnectedSocket::new(remote)?;

        socket.send_control(ConnectRequest::new(
            socket.local_hpai(),
            socket.local_hpai(),
            Cri::Tunnel(TunnelRequest::new(KnxLayer::LinkLayer)),
        ))?;

        let reply = socket
            .receive(CONNECT_REQUEST_TIMEOUT, ServiceType::ConnectResponse)?
            .unwrap_connect_response();
        let socket = socket.with_data(reply.data_endpoint.address);

        Ok(TunnelConnection {
            socket,
            channel_id: reply.communication_channel_id,
            last_keep_alive: Instant::now(),
        })
    }

    pub fn send_raw(&mut self, cemi: Cemi) -> Result<(), ConnectionError> {
        self.keep_alive()?;
        let result = self.socket.retry(1, |socket| {
            let data = match cookie_factory::gen_simple(cemi.gen(), vec![]) {
                Ok(data) => data,
                Err(e) => return Action::Finished(Err(e.into())),
            };
            if let Err(e) = socket.send_data(TunnelingRequest::new(self.channel_id, 0, data)) {
                return Action::Finished(Err(e));
            }

            let reply = match socket.receive(TUNNELING_REQUEST_TIMEOUT, ServiceType::TunnelResponse) {
                Ok(reply) => reply.unwrap_tunnel_ack(),
                Err(e) => return Action::Finished(Err(e)),
            };

            if reply.0.communication_channel_id != self.channel_id {
                warn!("Server sent connection state response for unexpected communication channel {} instead of {}.", reply.0.communication_channel_id, self.channel_id);
                return Action::Retry;
            }

            // TODO sequence number handling

            match reply.0.data() {
                Ok(TunnelingAckState::NoError) => Action::Finished(Ok(())),
                Err(state) => {
                    debug!(
                    "Server sent invalid tunneling ack state {state:?} for channel {}",
                    self.channel_id
                );
                Action::Retry
                }
            }
        });

        match result {
            Ok(r) => r,
            Err(_) => self.close(),
        }
    }

    pub fn receive_raw(&mut self, timeout: Duration) -> Result<Cemi, ConnectionError> {
        self.keep_alive()?;
        let request = self
            .socket
            .receive(timeout, ServiceType::TunnelRequest)?
            .unwrap_tunnel_request();
        if request.header.communication_channel_id != self.channel_id {
            warn!(
                "Server sent tunnel request for unexpected communication channel {} instead of {}.",
                request.header.communication_channel_id, self.channel_id
            );
            unimplemented!()
            //return Err(ConnectionError::IoError(std::io::Error::new(std::io::ErrorKind::)))
        }
        // TODO seq counter handling
        self.socket.send_data(TunnelingAck::new(
            self.channel_id,
            request.header.sequence_counter,
            TunnelingAckState::NoError,
        ))?;
        Ok(Cemi::parse(&request.cemi)?.1)
    }

    pub fn keep_alive(&mut self) -> Result<(), ConnectionError> {
        if self.last_keep_alive.elapsed() < CONNECTION_STATE_REQUEST_INTERVAL {
            return Ok(());
        }

        self.last_keep_alive = Instant::now();

        let result = self.socket.retry(3, |socket| {
            if let Err(e) = socket.send_control(ConnectionStateRequest::new(
                self.channel_id,
                socket.local_hpai(),
            )) {
                return Action::Finished(Err(e));
            }

            let reply = match socket.receive(CONNECTION_STATE_REQUEST_TIMEOUT, ServiceType::ConnectionStateResponse) {
                Err(e) => return Action::Finished(Err(e)),
                Ok(reply) => reply.unwrap_connection_state_response(),
            };

            if reply.communication_channel_id != self.channel_id {
                warn!("Server sent connection state response for unexpected communication channel {} instead of {}.", reply.communication_channel_id, self.channel_id);
                return Action::Retry;
            }

            match reply.state {
                ConnectionState::NoError => Action::Finished(Ok(())),
                state => {
                    debug!(
                    "Server sent connection state response with state {state:?} for channel {}",
                    self.channel_id
                );
                Action::Retry
                }
            }
        });

        match result {
            Err(_) => self.close(),
            Ok(r) => r,
        }
    }

    pub fn disconnect(mut self) -> Result<(), ConnectionError> {
        // must split into 2 function because drop only takes &mut self not self
        self.close()
    }

    fn close(&mut self) -> Result<(), ConnectionError> {
        self.socket.send_control(DisconnectRequest::new(
            self.channel_id,
            self.socket.local_hpai(),
        ))?;

        let reply = self
            .socket
            .receive(DISCONNECT_REQUEST_TIMEOUT, ServiceType::DisconnectResponse)?
            .unwrap_disconnect_response();

        if reply.communication_channel_id != self.channel_id {
            warn!("Server terminated connection for unexpected communication channel id {} instead of {}.", reply.communication_channel_id, self.channel_id);
        }

        if reply.state != DisconnectState::NoError {
            warn!(
                "Server indicated error {:?} while trying to disconnect.",
                reply.state
            );
        }

        Ok(())
    }
}

impl Drop for TunnelConnection {
    fn drop(&mut self) {
        match self.close() {
            Err(ConnectionError::IoError(e)) if e.kind() == std::io::ErrorKind::TimedOut => {}
            Ok(_) => {}
            Err(e) => warn!("Failed to close tunnel connection: {e}"),
        }
    }
}

enum Action<T> {
    Finished(T),
    Retry,
}

enum RetryResult<T> {
    RetriesExceeded,
    Result(T),
}

#[derive(Debug)]
struct UnconnectedSocket {
    socket: UdpSocket,
    local: SocketAddrV4,
    control: SocketAddrV4,
    buffer: VecDeque<Body>,
}

#[derive(Debug)]
struct Socket {
    socket: UnconnectedSocket,
    data: SocketAddrV4,
}

impl UnconnectedSocket {
    fn new(remote_control_endpoint: SocketAddrV4) -> io::Result<Self> {
        let socket = UdpSocket::bind(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 0))?;

        let local = match socket.local_addr()? {
            SocketAddr::V4(ep) => ep,
            SocketAddr::V6(ep) => {
                panic!("Bound UdpSocket to IpV4 addr but it reported being bound to {ep}");
            }
        };

        Ok(UnconnectedSocket {
            socket,
            local,
            buffer: VecDeque::new(),
            control: remote_control_endpoint,
        })
    }

    fn with_data(self, remote_data_endpoint: SocketAddrV4) -> Socket {
        Socket {
            socket: self,
            data: remote_data_endpoint,
        }
    }

    fn send(&self, body: Body, endpoint: SocketAddrV4) -> Result<(), ConnectionError> {
        let frame = Frame::wrap(body);
        trace!("Sending {frame:?} to {endpoint}.");
        let buf = cookie_factory::gen_simple(frame.gen(), vec![])?;
        self.socket.send_to(&buf, endpoint)?;
        Ok(())
    }

    fn receive(
        &mut self,
        timeout: Duration,
        service: ServiceType,
    ) -> Result<Body, ConnectionError> {
        self.receive_from(timeout, service, &[self.control])
    }

    fn receive_from(
        &mut self,
        timeout: Duration,
        service: ServiceType,
        endpoints: &[SocketAddrV4],
    ) -> Result<Body, ConnectionError> {
        if let Some(index) = self
            .buffer
            .iter()
            .position(|body| body.as_service_type() == service)
        {
            return Ok(self
                .buffer
                .remove(index)
                .expect("invalid index but just found it in buffer"));
        }

        let start = Instant::now();
        let mut remainder = timeout;

        while start.elapsed() <= timeout {
            let mut buf = [0; u16::MAX as usize];
            self.socket.set_read_timeout(Some(remainder))?;
            let (size, sender) = self.socket.recv_from(&mut buf[..])?;

            if size > buf.len() {
                warn!(
                    "Message larger than provided buffer. Lost {} bytes of data.",
                    size - buf.len()
                );
            }

            match sender {
                SocketAddr::V4(addr) if endpoints.contains(&addr) => {
                    let frame = Frame::parse(&buf[..size])?.1;
                    trace!("Received {frame:?} from {addr}.");
                    if frame.body.as_service_type() == service {
                        return Ok(frame.body);
                    }
                    self.buffer.push_back(frame.body);
                }
                addr => {
                    warn!(
                        "Ignoring message from wrong endpoint {addr}: {:?}",
                        &buf[..size]
                    );
                }
            }

            remainder -= start.elapsed();
        }

        Err(io::Error::new(
            io::ErrorKind::TimedOut,
            format!("Receive timeout of {} seconds exceeded", timeout.as_secs()),
        )
        .into())
    }

    fn send_control(&self, body: impl Into<Body>) -> Result<(), ConnectionError> {
        self.send(body.into(), self.control)
    }

    fn local_hpai(&self) -> Hpai {
        Hpai::new(HostProtocolCode::Ipv4Udp, self.local)
    }
}

impl Socket {
    fn send_control(&self, body: impl Into<Body>) -> Result<(), ConnectionError> {
        self.socket.send(body.into(), self.socket.control)
    }

    fn send_data(&self, body: impl Into<Body>) -> Result<(), ConnectionError> {
        self.socket.send(body.into(), self.data)
    }

    fn receive(
        &mut self,
        timeout: Duration,
        service: ServiceType,
    ) -> Result<Body, ConnectionError> {
        self.socket
            .receive_from(timeout, service, &[self.data, self.socket.control])
    }

    fn local_hpai(&self) -> Hpai {
        self.socket.local_hpai()
    }

    fn retry<F, T>(&mut self, max_retries: u32, mut f: F) -> Result<T, ()>
    where
        F: FnMut(&mut Self) -> Action<T>,
    {
        for _ in 0..=max_retries {
            if let Action::Finished(r) = f(self) {
                return Ok(r);
            }
        }
        Err(())
    }
}
