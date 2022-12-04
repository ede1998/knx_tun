use std::{
    sync::{Arc, Mutex},
    time::Duration,
};

use thiserror::Error;

use super::connection::{ConnectionError, TunnelConnection};
use crate::cemi::Cemi;

#[derive(Debug, Error)]
#[error("Sender and receiver belong to different connections.")]
pub struct SenderReceiverMismatch {
    sender: Sender,
    receiver: Receiver,
}

#[derive(Debug)]
pub struct Receiver {
    tunnel: Arc<Mutex<TunnelConnection>>,
    timeout: Duration,
}

impl Iterator for Receiver {
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

#[derive(Debug)]
pub struct Sender {
    tunnel: Arc<Mutex<TunnelConnection>>,
}

impl Sender {
    pub fn send_raw(&self, cemi: Cemi) -> Result<(), ConnectionError> {
        let mut guard = self.tunnel.lock().expect("ensure mutex not poisoned");
        guard.send_raw(cemi)
    }
}

impl TunnelConnection {
    pub fn bidirectional(
        self,
        receive_timeout: Duration,
    ) -> Result<(Sender, Receiver), (Box<Self>, ConnectionError)> {
        if !self.state.is_connected() {
            let err = ConnectionError::invalid_operation(stringify!(bidirectional), &self.state);
            return Err((self.into(), err));
        };

        let tunnel = Arc::new(Mutex::new(self));
        let sender = Sender {
            tunnel: tunnel.clone(),
        };
        let receiver = Receiver {
            tunnel,
            timeout: receive_timeout,
        };

        Ok((sender, receiver))
    }

    pub fn from_bidirectional(
        sender: Sender,
        receiver: Receiver,
    ) -> Result<Self, SenderReceiverMismatch> {
        if !Arc::ptr_eq(&sender.tunnel, &receiver.tunnel) {
            return Err(SenderReceiverMismatch { sender, receiver });
        }

        drop(receiver);

        Ok(Arc::try_unwrap(sender.tunnel)
            .expect("only 1 of 2 owners left: sender because receiver was dropped")
            .into_inner()
            .expect("ensure mutex not poisoned"))
    }
}
