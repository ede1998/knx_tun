use std::{
    collections::VecDeque,
    io,
    net::{Ipv4Addr, SocketAddr, SocketAddrV4, UdpSocket},
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

#[derive(Debug, Clone, Copy)]
enum KeepAliveState {
    Assessing,
    Alive { last_check: Instant },
}

impl Default for KeepAliveState {
    fn default() -> Self {
        Self::Alive {
            last_check: Instant::now(),
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
enum TunnelSendState {
    AwaitingReply {
        sequence_counter: u8,
    },
    #[default]
    Ready,
}

enum ReplyAction {
    ProcessAndAck,
    DiscardOnly { expected: u8 },
    DiscardAndAck { expected: u8 },
}

#[derive(Debug, Clone, Copy)]
struct Channel {
    id: u8,
    recv_sequence: std::num::Wrapping<u8>,
    send_sequence: std::num::Wrapping<u8>,
}

impl Channel {
    const fn new(id: u8) -> Self {
        Self {
            id,
            recv_sequence: std::num::Wrapping(0),
            send_sequence: std::num::Wrapping(0),
        }
    }

    const fn id(&self) -> u8 {
        self.id
    }

    fn next_send_sequence(&mut self) -> u8 {
        let seq = self.send_sequence;
        self.send_sequence += 1;
        seq.0
    }

    fn verify_recv_sequence(&mut self, received: u8) -> ReplyAction {
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

#[derive(Debug, Clone, Copy)]
struct Connected {
    pub tunnel: TunnelSendState,
    pub keep_alive: KeepAliveState,
    pub control: SocketAddrV4,
    pub data: SocketAddrV4,
    pub channel: Channel,
}

impl Connected {
    fn to_disconnecting(self) -> Disconnecting {
        let Self {
            control,
            channel,
            data,
            ..
        } = self;
        Disconnecting {
            control,
            channel,
            data,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Disconnecting {
    pub control: SocketAddrV4,
    pub data: SocketAddrV4,
    pub channel: Channel,
}

#[derive(Debug, Clone)]
enum ConnectionState {
    NotConnected,
    Connecting { control: SocketAddrV4 },
    Connected(Connected),
    Disconnecting(Disconnecting),
}

impl Default for ConnectionState {
    fn default() -> Self {
        Self::NotConnected
    }
}

impl ConnectionState {
    fn connected(channel_id: u8, control: SocketAddrV4, data: SocketAddrV4) -> Self {
        Self::Connected(Connected {
            tunnel: TunnelSendState::default(),
            keep_alive: KeepAliveState::default(),
            control,
            data,
            channel: Channel::new(channel_id),
        })
    }
}

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
        if !matches!(self.state, ConnectionState::NotConnected) {
            panic!("TODO already connected");
        }

        let frame = serialize(ConnectRequest::new(
            hpai(self.local),
            hpai(self.local),
            Cri::Tunnel(TunnelRequest::new(KnxLayer::LinkLayer)),
        ))?;

        self.socket.send_to(&frame, remote)?;
        self.state = ConnectionState::Connecting { control: remote };
        self.maintain_until(CONNECT_REQUEST_TIMEOUT, Self::is_connected)?;
        Ok(())
    }

    pub fn send_raw(&mut self, cemi: Cemi) -> Result<(), ConnectionError> {
        let (frame, dest) = match self.state {
            ConnectionState::Connected(ref mut state) => {
                if let TunnelSendState::AwaitingReply { .. } = state.tunnel {
                    panic!("TODO: handle sendstate awaiting reply")
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
                state.tunnel = TunnelSendState::AwaitingReply { sequence_counter };

                (frame, data)
            }
            _ => panic!("TODO: handle not connected/connecting/disconnecting"),
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
        let ConnectionState::Connected {..} = self.state else {
            panic!("TODO: handle not connected/connecting/disconnecting");
        };

        match self.buffer.pop_front() {
            Some(cemi) => {
                self.maintain(Duration::ZERO)?;
                Ok(cemi)
            }
            None => self
                .maintain_until(timeout, Self::received_cemi)
                .and_then(|_received_cemi| {
                    self.buffer.pop_front().ok_or_else(|| {
                        ConnectionError::IoError(io::Error::new(
                            io::ErrorKind::TimedOut,
                            "Did not receive CEMI message within set timeout.",
                        ))
                    })
                }),
        }
    }

    pub fn keep_alive(&mut self) -> Result<(), ConnectionError> {
        let (frame, dest) = match self.state {
            ConnectionState::Connected(ref mut state) => {
                let KeepAliveState::Alive { last_check } = state.keep_alive else {
                    panic!("TODO: invalid keep alive state");
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
                state.keep_alive = KeepAliveState::Assessing;

                (frame, control)
            }
            ConnectionState::NotConnected => panic!("Not connected yet TODO"),
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
            _ => panic!("TODO: handle not connected/connecting/disconnecting"),
        };
        let frame = serialize(DisconnectRequest::new(state.channel.id(), hpai(self.local)))?;
        self.socket.send_to(&frame, state.control)?;
        self.state = ConnectionState::Disconnecting(state.to_disconnecting());
        if self.maintain_until(CONNECT_REQUEST_TIMEOUT, Self::is_disconnected)? {
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::TimedOut,
                "Did not receive disconnect response within timeout.",
            )
            .into())
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
            keep_alive: KeepAliveState::Alive { .. },
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
                ConnectionState::Connecting { control, .. } => {
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
            (ConnectionState::Connecting { control }, Body::ConnectResponse(r)) => {
                self.state = ConnectionState::connected(
                    r.communication_channel_id,
                    *control,
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
                    ReplyAction::ProcessAndAck => {
                        let cemi = Cemi::parse(&r.cemi)?.1;
                        self.buffer.push_back(cemi);
                        send_ack()?;
                    }
                    ReplyAction::DiscardOnly { expected } => {
                        debug!("Discarding message because of unexpected sequence number {}. Expected {expected}.", r.header.communication_channel_id);
                    }
                    ReplyAction::DiscardAndAck { expected } => {
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
                    TunnelSendState::Ready => {
                        debug!("Ignoring tunneling ack because tunnel has no outstanding ack.");
                    }
                    TunnelSendState::AwaitingReply { sequence_counter } => {
                        if a.0.sequence_counter != *sequence_counter {
                            debug!("Ignoring tunneling ack because it is for the sequence counter {} instead of {sequence_counter}.", a.0.sequence_counter);
                        } else {
                            *tunnel = TunnelSendState::Ready;
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
                    KeepAliveState::Assessing => {
                        *keep_alive = KeepAliveState::default();
                    }
                    KeepAliveState::Alive { .. } => {
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
                tunnel: TunnelSendState::Ready,
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
                keep_alive: KeepAliveState::Alive { .. },
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
            Err(ConnectionError::IoError(e)) if e.kind() == std::io::ErrorKind::TimedOut => {}
            Ok(_) => {}
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
