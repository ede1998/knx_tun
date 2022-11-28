use std::{
    collections::VecDeque,
    io,
    net::{Ipv4Addr, SocketAddr, SocketAddrV4, UdpSocket},
    sync::{Arc, Mutex},
    time::{Duration, Instant},
};

use cookie_factory::GenError;
use thiserror::Error;
use tracing::{debug, trace, warn};

use crate::{
    cemi::Cemi,
    connect::{ConnectRequest, Cri, KnxLayer, TunnelRequest},
    core::{Body, Frame},
    disconnect::{DisconnectRequest, DisconnectResponse, DisconnectState},
    hpai::{HostProtocolCode, Hpai},
    keep_alive::ConnectionStateRequest,
    snack::{In, NomErr},
    tunneling::{TunnelingAck, TunnelingAckState, TunnelingRequest},
};

#[derive(Clone, Copy, Debug)]
pub enum State {
    NotConnected,
    Connecting,
    Connected,
    Disconnecting,
}

impl From<&ConnectionState> for State {
    fn from(f: &ConnectionState) -> Self {
        match f {
            ConnectionState::NotConnected => Self::NotConnected,
            ConnectionState::Connecting(_) => Self::Connecting,
            ConnectionState::Connected(_) => Self::Connected,
            ConnectionState::Disconnecting(_) => Self::Disconnecting,
        }
    }
}

#[derive(Debug, Error)]
pub enum ConnectionError {
    #[error("network error: {0}")]
    IoError(#[from] io::Error),
    #[error("could not serialize packet: {0}")]
    GenError(#[from] cookie_factory::GenError),
    #[error("could not parse packet: {0:?}")]
    ParseError(#[from] NomErr<Vec<u8>>),
    #[error("Did not receive reply in time: {0}")]
    TimeoutError(&'static str),
    #[error("Method {0} is not allowed to be called in state {1:?}")]
    InvalidOperation(&'static str, State),
}

impl<'a> From<NomErr<In<'a>>> for ConnectionError {
    fn from(f: NomErr<In<'a>>) -> Self {
        Self::ParseError(NomErr::<Vec<u8>>::convert(f))
    }
}

impl ConnectionError {
    fn invalid_operation(name: &'static str, state: &ConnectionState) -> Self {
        ConnectionError::InvalidOperation(name, state.into())
    }
}

mod states {
    use std::{net::SocketAddrV4, time::Instant};

    #[derive(Debug, Clone, Copy)]
    pub enum KeepAlive {
        Assessing,
        Alive { last_check: Instant },
    }

    impl Default for KeepAlive {
        fn default() -> Self {
            Self::Alive {
                last_check: Instant::now(),
            }
        }
    }

    #[derive(Default, Debug, Clone, Copy)]
    pub enum TunnelSend {
        AwaitingReply {
            sequence_counter: u8,
        },
        #[default]
        Ready,
    }

    pub enum ReplyAction {
        ProcessAndAck,
        DiscardOnly { expected: u8 },
        DiscardAndAck { expected: u8 },
    }

    #[derive(Debug, Clone)]
    pub struct Channel {
        id: u8,
        recv_sequence: std::num::Wrapping<u8>,
        send_sequence: std::num::Wrapping<u8>,
    }

    impl Channel {
        pub const fn new(id: u8) -> Self {
            Self {
                id,
                recv_sequence: std::num::Wrapping(0),
                send_sequence: std::num::Wrapping(0),
            }
        }

        pub const fn id(&self) -> u8 {
            self.id
        }

        pub fn next_send_sequence(&mut self) -> u8 {
            let seq = self.send_sequence;
            self.send_sequence += 1;
            seq.0
        }

        pub fn verify_recv_sequence(&mut self, received: u8) -> ReplyAction {
            let expected = self.recv_sequence.0;
            let expected_sub_1 = (self.recv_sequence - std::num::Wrapping(1)).0;

            if received == expected {
                self.recv_sequence += 1;
                ReplyAction::ProcessAndAck
            } else if received == expected_sub_1 {
                ReplyAction::DiscardAndAck { expected }
            } else {
                ReplyAction::DiscardOnly { expected }
            }
        }
    }

    #[derive(Debug, Clone)]
    pub struct Connected {
        pub tunnel: TunnelSend,
        pub keep_alive: KeepAlive,
        pub control: SocketAddrV4,
        pub data: SocketAddrV4,
        pub channel: Channel,
    }

    #[derive(Debug, Clone)]
    pub struct Disconnecting {
        pub control: SocketAddrV4,
        pub data: SocketAddrV4,
        pub channel: Channel,
    }

    #[derive(Debug, Clone)]
    pub struct Connecting {
        pub control: SocketAddrV4,
    }

    #[derive(Debug, Clone)]
    pub enum ConnectionState {
        NotConnected,
        Connecting(Connecting),
        Connected(Connected),
        Disconnecting(Disconnecting),
    }

    impl Default for ConnectionState {
        fn default() -> Self {
            Self::NotConnected
        }
    }

    impl ConnectionState {
        pub fn connected(channel_id: u8, control: SocketAddrV4, data: SocketAddrV4) -> Self {
            Self::Connected(Connected {
                tunnel: TunnelSend::default(),
                keep_alive: KeepAlive::default(),
                control,
                data,
                channel: Channel::new(channel_id),
            })
        }

        pub fn disconnecting(connected: &Connected) -> Self {
            Self::Disconnecting(Disconnecting {
                control: connected.control,
                channel: connected.channel.clone(),
                data: connected.data,
            })
        }

        /// Returns `true` if the connection state is [`Disconnecting`].
        ///
        /// [`Disconnecting`]: ConnectionState::Disconnecting
        #[must_use]
        pub fn is_disconnecting(&self) -> bool {
            matches!(self, Self::Disconnecting(..))
        }

        /// Returns `true` if the connection state is [`Connecting`].
        ///
        /// [`Connecting`]: ConnectionState::Connecting
        #[must_use]
        pub fn is_connecting(&self) -> bool {
            matches!(self, Self::Connecting(..))
        }

        /// Returns `true` if the connection state is [`NotConnected`].
        ///
        /// [`NotConnected`]: ConnectionState::NotConnected
        #[must_use]
        pub fn is_not_connected(&self) -> bool {
            matches!(self, Self::NotConnected)
        }

        /// Returns `true` if the connection state is [`Connected`].
        ///
        /// [`Connected`]: ConnectionState::Connected
        #[must_use]
        pub fn is_connected(&self) -> bool {
            matches!(self, Self::Connected(..))
        }
    }
}

pub struct Receiver<'a> {
    tunnel: Arc<Mutex<&'a mut TunnelConnection>>,
    timeout: Duration,
}

impl<'a> Iterator for Receiver<'a> {
    type Item = Option<Cemi>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut guard = self.tunnel.lock().expect("ensure mutex not poisoned");
        match guard.receive_raw(self.timeout) {
            Ok(cemi) => Some(Some(cemi)),
            Err(ConnectionError::TimeoutError(_)) => Some(None),
            _ => None,
        }
    }
}

pub struct Sender<'a> {
    tunnel: Arc<Mutex<&'a mut TunnelConnection>>,
}

impl<'a> Sender<'a> {
    pub fn send_raw(&self, cemi: Cemi) -> Result<(), ConnectionError> {
        let mut guard = self.tunnel.lock().expect("ensure mutex not poisoned");
        guard.send_raw(cemi)
    }
}

use states::{Connected, Connecting, ConnectionState, Disconnecting};

#[derive(Debug)]
pub struct TunnelConnection {
    state: ConnectionState,
    socket: UdpSocket,
    local: SocketAddrV4,
    buffer: VecDeque<Cemi>,
}

const CONNECT_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);
const CONNECTION_STATE_REQUEST_INTERVAL: Duration = Duration::from_secs(2);
const CONNECTION_STATE_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);
const DISCONNECT_REQUEST_TIMEOUT: Duration = Duration::from_secs(10);
const TUNNELING_REQUEST_TIMEOUT: Duration = Duration::from_secs(1);

impl TunnelConnection {
    pub fn bidirectional(
        &mut self,
        receive_timeout: Duration,
    ) -> Result<(Sender, Receiver), ConnectionError> {
        if !self.state.is_connected() {
            return Err(ConnectionError::invalid_operation(
                stringify!(bidirectional),
                &self.state,
            ));
        };

        let borrow = Arc::new(Mutex::new(self));
        let sender = Sender {
            tunnel: borrow.clone(),
        };
        let receiver = Receiver {
            tunnel: borrow,
            timeout: receive_timeout,
        };

        Ok((sender, receiver))
    }
    pub fn new() -> Result<Self, ConnectionError> {
        let (socket, local) = bind()?;
        Ok(Self {
            state: Default::default(),
            socket,
            local,
            buffer: VecDeque::new(),
        })
    }

    pub fn open(&mut self, remote: SocketAddrV4) -> Result<(), ConnectionError> {
        if !self.state.is_not_connected() {
            return Err(ConnectionError::invalid_operation(
                stringify!(open),
                &self.state,
            ));
        }

        let frame = serialize(ConnectRequest::new(
            hpai(self.local),
            hpai(self.local),
            Cri::Tunnel(TunnelRequest::new(KnxLayer::LinkLayer)),
        ))?;

        self.socket.send_to(&frame, remote)?;
        self.state = ConnectionState::Connecting(Connecting { control: remote });
        if self.maintain_until(CONNECT_REQUEST_TIMEOUT, Self::is_connected)? {
            Ok(())
        } else {
            self.state = ConnectionState::NotConnected;
            Err(ConnectionError::TimeoutError("Failed to open connection"))
        }
    }

    pub fn send_raw(&mut self, cemi: Cemi) -> Result<(), ConnectionError> {
        let (frame, dest) = match self.state {
            ConnectionState::Connected(ref mut state) => {
                if let states::TunnelSend::AwaitingReply { .. } = state.tunnel {
                    // This cannot happen (because of the exclusive reference and
                    // because if no ack is received, the connection would have been closed
                    // before returning control to the caller) UNLESS sending/receiving
                    // errors out while waiting for the ack.
                    // It's also not very important to keep this around as we only log
                    // that the ack was unexpected if it's received later.
                    debug!("Resetting left-over TunnelSend::AwaitingReply flag.");
                    state.tunnel = states::TunnelSend::Ready;
                }
                trace!("Serializing {cemi:?}.");
                let cemi = cookie_factory::gen_simple(cemi.gen(), vec![])?;

                let sequence_counter = state.channel.next_send_sequence();
                let frame = serialize(TunnelingRequest::new(
                    state.channel.id(),
                    sequence_counter,
                    cemi,
                ))?;

                let data = state.data;

                self.socket.send_to(&frame, data)?;
                state.tunnel = states::TunnelSend::AwaitingReply { sequence_counter };

                (frame, data)
            }
            ref state => {
                return Err(ConnectionError::invalid_operation(
                    stringify!(send_raw),
                    state,
                ))
            }
        };

        if self.maintain_until(TUNNELING_REQUEST_TIMEOUT, Self::is_tunnel_ready)? {
            return Ok(());
        }
        self.socket.send_to(&frame, dest)?;
        if self.maintain_until(TUNNELING_REQUEST_TIMEOUT, Self::is_tunnel_ready)? {
            Ok(())
        } else {
            self.close()
        }
    }

    pub fn receive_raw(&mut self, timeout: Duration) -> Result<Cemi, ConnectionError> {
        if !self.state.is_connected() {
            return Err(ConnectionError::invalid_operation(
                stringify!(receive_raw),
                &self.state,
            ));
        }

        match self.buffer.pop_front() {
            Some(cemi) => {
                self.maintain(Duration::ZERO)?;
                Ok(cemi)
            }
            None => {
                self.maintain_until(timeout, Self::received_cemi)?;
                self.buffer.pop_front().ok_or(ConnectionError::TimeoutError(
                    "Did not receive CEMI message within set timeout.",
                ))
            }
        }
    }

    pub fn keep_alive(&mut self) -> Result<(), ConnectionError> {
        let (frame, dest) = match self.state {
            ConnectionState::Connected(ref mut state) => {
                let states::KeepAlive::Alive { last_check } = state.keep_alive else {
                    // Still waiting for a keep alive should not happen. Something
                    // went wrong in a previous call, maybe a send/recv error'ed
                    // out after sending the keep alive
                    debug!("Resetting left-over KeepAlive::Assessing flag.");
                    state.keep_alive = states::KeepAlive::default();
                    return Ok(());
                };

                if last_check.elapsed() < CONNECTION_STATE_REQUEST_INTERVAL {
                    return Ok(());
                }

                let frame = serialize(ConnectionStateRequest::new(
                    state.channel.id(),
                    hpai(self.local),
                ))?;

                let control = state.control;

                self.socket.send_to(&frame, control)?;
                state.keep_alive = states::KeepAlive::Assessing;

                (frame, control)
            }
            ConnectionState::NotConnected => {
                return Err(ConnectionError::invalid_operation(
                    stringify!(keep_alive),
                    &self.state,
                ))
            }
            _ => {
                return Ok(());
            }
        };

        for _ in 0..3 {
            if self.maintain_until(CONNECTION_STATE_REQUEST_TIMEOUT, Self::still_alive)? {
                return Ok(());
            }
            self.socket.send_to(&frame, dest)?;
        }

        self.close()
    }

    pub fn close(&mut self) -> Result<(), ConnectionError> {
        let state = match self.state {
            ConnectionState::Connected(ref mut state) => state,
            ConnectionState::NotConnected => return Ok(()),
            ref state => return Err(ConnectionError::invalid_operation(stringify!(close), state)),
        };
        let frame = serialize(DisconnectRequest::new(state.channel.id(), hpai(self.local)))?;
        self.socket.send_to(&frame, state.control)?;
        self.state = ConnectionState::disconnecting(state);
        if self.maintain_until(CONNECT_REQUEST_TIMEOUT, Self::is_disconnected)? {
            Ok(())
        } else {
            Err(ConnectionError::TimeoutError(
                "Did not receive disconnect response within timeout.",
            ))
        }
    }

    fn maintain_until<F>(
        &mut self,
        timeout: Duration,
        state_reached: F,
    ) -> Result<bool, ConnectionError>
    where
        F: Fn(&Self) -> bool,
    {
        let start = Instant::now();
        let mut remaining_time = Duration::MAX; // ensure one iteration
        while remaining_time > Duration::ZERO {
            remaining_time = timeout.saturating_sub(start.elapsed());
            self.maintain(remaining_time)?;
            if state_reached(self) {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn maintain(&mut self, timeout: Duration) -> Result<(), ConnectionError> {
        if let ConnectionState::Connected(Connected {
            keep_alive: states::KeepAlive::Alive { .. },
            ..
        }) = self.state
        {
            self.keep_alive()?;
        }

        let mut buf = [0; 1000];
        let start = Instant::now();
        let mut remaining_time = Duration::MAX;
        let frame = loop {
            if remaining_time.is_zero() {
                return Ok(());
            };
            remaining_time = timeout.saturating_sub(start.elapsed());

            let msg_len = match self.state {
                ConnectionState::NotConnected => return Ok(()),
                ConnectionState::Connecting(Connecting { control, .. }) => {
                    receive(&self.socket, &mut buf, &[control], remaining_time)?
                }
                ConnectionState::Connected(Connected { control, data, .. })
                | ConnectionState::Disconnecting(Disconnecting { control, data, .. }) => {
                    receive(&self.socket, &mut buf, &[control, data], remaining_time)?
                }
            };

            let Some(msg_len) = msg_len else { continue; };

            match Frame::parse(&buf[..msg_len]) {
                Ok((_, frame)) => break frame,
                Err(err) => {
                    debug!("Failed to parse incoming frame: {}", err);
                    continue;
                }
            };
        };

        trace!("Received {frame:?}");

        self.handle_message(frame)
    }

    fn handle_message(&mut self, frame: Frame) -> Result<(), ConnectionError> {
        match (&mut self.state, frame.body) {
            (ConnectionState::Connecting(connected), Body::ConnectResponse(r)) => {
                self.state = ConnectionState::connected(
                    r.communication_channel_id,
                    connected.control,
                    r.data_endpoint.address,
                )
            }
            (
                ConnectionState::Connected(Connected {
                    data,
                    ref mut channel,
                    ..
                }),
                Body::TunnelRequest(r),
            ) => {
                if channel.id() != r.header.communication_channel_id {
                    debug!(
                        "Ignoring tunneling request for unknown channel id {}. Expected {}.",
                        channel.id(),
                        r.header.communication_channel_id
                    );
                    return Ok(());
                }

                let channel_id = channel.id();
                let send_ack = || -> Result<(), ConnectionError> {
                    let frame = serialize(TunnelingAck::new(
                        channel_id,
                        r.header.sequence_counter,
                        TunnelingAckState::NoError,
                    ))?;

                    self.socket.send_to(&frame, *data)?;
                    Ok(())
                };

                match channel.verify_recv_sequence(r.header.sequence_counter) {
                    states::ReplyAction::ProcessAndAck => {
                        let cemi = Cemi::parse(&r.cemi)?.1;
                        self.buffer.push_back(cemi);
                        send_ack()?;
                    }
                    states::ReplyAction::DiscardOnly { expected } => {
                        debug!("Discarding message because of unexpected sequence number {}. Expected {expected}.", r.header.communication_channel_id);
                    }
                    states::ReplyAction::DiscardAndAck { expected } => {
                        debug!("Discarding message but sending ack because of unexpected sequence number {} is one less than expected value {expected}.", r.header.communication_channel_id);
                        send_ack()?;
                    }
                }
            }
            (
                ConnectionState::Connected(Connected {
                    ref mut tunnel,
                    channel,
                    ..
                }),
                Body::TunnelAck(a),
            ) => {
                if channel.id() != a.0.communication_channel_id {
                    debug!(
                        "Ignoring tunneling request for unknown channel id {}. Expected {}.",
                        channel.id(),
                        a.0.communication_channel_id
                    );
                    return Ok(());
                }

                match tunnel {
                    states::TunnelSend::Ready => {
                        debug!("Ignoring tunneling ack because tunnel has no outstanding ack.");
                    }
                    states::TunnelSend::AwaitingReply { sequence_counter } => {
                        if a.0.sequence_counter != *sequence_counter {
                            debug!("Ignoring tunneling ack because it is for the sequence counter {} instead of {sequence_counter}.", a.0.sequence_counter);
                        } else {
                            *tunnel = states::TunnelSend::Ready;
                        }
                    }
                }
            }
            (ConnectionState::Connected(Connected { channel, .. }), Body::DisconnectRequest(r)) => {
                if channel.id() != r.communication_channel_id {
                    debug!(
                        "Ignoring tunneling request for unknown channel id {}. Expected {}.",
                        channel.id(),
                        r.communication_channel_id
                    );
                    return Ok(());
                }
                let channel_id = channel.id();
                self.state = ConnectionState::NotConnected;
                let frame = serialize(DisconnectResponse::new(
                    channel_id,
                    DisconnectState::NoError,
                ))?;
                self.socket.send_to(&frame, r.control_endpoint.address)?;
            }
            (
                ConnectionState::Disconnecting(Disconnecting { channel, .. }),
                Body::DisconnectResponse(r),
            ) => {
                if channel.id() != r.communication_channel_id {
                    debug!(
                        "Ignoring tunneling response for unknown channel id {}. Expected {}.",
                        channel.id(),
                        r.communication_channel_id
                    );
                    return Ok(());
                }
                self.state = ConnectionState::NotConnected;
            }
            (
                ConnectionState::Connected(Connected {
                    ref mut keep_alive,
                    channel,
                    ..
                }),
                Body::ConnectionStateResponse(r),
            ) => {
                if channel.id() != r.communication_channel_id {
                    debug!(
                        "Ignoring tunneling response for unknown channel id {}. Expected {}.",
                        channel.id(),
                        r.communication_channel_id
                    );
                    return Ok(());
                }
                match keep_alive {
                    states::KeepAlive::Assessing => {
                        *keep_alive = states::KeepAlive::default();
                    }
                    states::KeepAlive::Alive { .. } => {
                        debug!("Ignoring unexpected connection state response while not waiting for one.");
                    }
                }
            }
            (state, body) => {
                debug!("Ignoring unexpectedly received {body:?} while in state {state:?}")
            }
        }
        Ok(())
    }

    fn is_tunnel_ready(&self) -> bool {
        matches!(
            self.state,
            ConnectionState::Connected(Connected {
                tunnel: states::TunnelSend::Ready,
                ..
            })
        )
    }

    fn received_cemi(&self) -> bool {
        !self.buffer.is_empty()
    }

    fn still_alive(&self) -> bool {
        matches!(
            self.state,
            ConnectionState::Connected(Connected {
                keep_alive: states::KeepAlive::Alive { .. },
                ..
            })
        )
    }

    fn is_disconnected(&self) -> bool {
        matches!(self.state, ConnectionState::NotConnected)
    }

    fn is_connected(&self) -> bool {
        matches!(self.state, ConnectionState::Connected { .. })
    }
}

impl Drop for TunnelConnection {
    fn drop(&mut self) {
        match self.close() {
            Err(ConnectionError::TimeoutError(_)) | Ok(_) => {}
            Err(e) => warn!("Failed to close tunnel connection: {e}"),
        }
    }
}

fn hpai(address: SocketAddrV4) -> Hpai {
    Hpai {
        protocol_code: HostProtocolCode::Ipv4Udp,
        address,
    }
}

fn receive(
    socket: &UdpSocket,
    buf: &mut [u8],
    endpoints: &[SocketAddrV4],
    timeout: Duration,
) -> Result<Option<usize>, std::io::Error> {
    let timeout = timeout.max(Duration::new(0, 1));
    socket.set_read_timeout(Some(timeout))?;
    match socket.recv_from(buf) {
        Ok((len, SocketAddr::V4(sender))) if endpoints.contains(&sender) => Ok(Some(len)),
        Ok((len, sender)) => {
            debug!("Ignoring message of {len} bytes from unexpected sender {sender}.");
            Ok(None)
        }
        Err(e) => {
            let kind = e.kind();
            if kind == io::ErrorKind::TimedOut || kind == io::ErrorKind::WouldBlock {
                Ok(None)
            } else {
                Err(e)
            }
        }
    }
}

fn bind() -> std::io::Result<(UdpSocket, SocketAddrV4)> {
    let socket = UdpSocket::bind(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 0))?;

    let local = match socket.local_addr()? {
        SocketAddr::V4(ep) => ep,
        SocketAddr::V6(ep) => {
            panic!("Bound UdpSocket to IpV4 addr but it reported being bound to {ep}");
        }
    };
    Ok((socket, local))
}

fn serialize<B: Into<Body>>(body: B) -> Result<Vec<u8>, GenError> {
    let frame = Frame::wrap(body.into());
    trace!("Serializing {frame:?}.");
    cookie_factory::gen_simple(frame.gen(), vec![])
}
