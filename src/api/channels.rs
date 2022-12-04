use std::{
    marker::PhantomData,
    sync::{Arc, Mutex},
    time::Duration,
};

use thiserror::Error;
use tracing::warn;

use super::connection::{ConnectionError, TunnelConnection};
use crate::{
    address::{GroupAddress, RawAddress},
    cemi::Cemi,
    dpt::general::DataPointType,
};

#[derive(Debug, Error)]
#[error("Sender and receiver belong to different connections.")]
pub struct SenderReceiverMismatch<T> {
    sender: Sender,
    receiver: Receiver<T>,
}

#[derive(Debug)]
pub struct Receiver<T> {
    tunnel: Arc<Mutex<TunnelConnection>>,
    timeout: Duration,
    dummy: PhantomData<T>,
}

impl<T> Receiver<T> {
    fn recv_cemi(&self) -> Option<Option<Cemi>> {
        let mut guard = self.tunnel.lock().expect("ensure mutex not poisoned");
        let cemi = guard.receive_raw(self.timeout);
        drop(guard);
        match cemi {
            Ok(cemi) => Some(Some(cemi)),
            Err(ConnectionError::TimeoutError(_)) => Some(None),
            Err(err) => {
                warn!("Failed to receive message: {err}");
                None
            }
        }
    }
}

impl Iterator for Receiver<Cemi> {
    type Item = Option<Cemi>;

    fn next(&mut self) -> Option<Self::Item> {
        self.recv_cemi()
    }
}

impl<D> Iterator for Receiver<GroupMessage<D>>
where
    D: DataPointType,
    for<'a> D::ParseError<'a>: std::fmt::Display,
{
    type Item = Option<GroupMessage<D>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.recv_cemi().and_then(|o| {
            o.and_then(|cemi| match GroupMessage::from_cemi(&cemi) {
                Some(Ok(message)) => Some(Some(message)),
                Some(Err(err)) => {
                    warn!("Failed to parse {cemi:?} as {}: {err}", D::id());
                    None
                }
                None => None,
            })
        })
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

    pub fn send<T>(&self, group_message: T, address: GroupAddress) -> Result<(), ConnectionError>
    where
        T: DataPointType,
    {
        use crate::cemi::{
            AdditionalInformation, Apdu, CemiBody, CemiHeader, LData, MessageCode, Tpdu,
        };
        let cemi = Cemi {
            header: CemiHeader {
                message_code: MessageCode::LDataReq,
                additional_info: AdditionalInformation,
            },
            body: CemiBody::LData(LData::new(
                RawAddress::ZERO,
                address.into(),
                Tpdu::DataGroup(Apdu::GroupValueWrite(group_message.to_data())),
            )),
        };

        self.send_raw(cemi)
    }
}

impl TunnelConnection {
    pub fn bidirectional<T>(
        self,
        receive_timeout: Duration,
    ) -> Result<(Sender, Receiver<T>), (Box<Self>, ConnectionError)> {
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
            dummy: PhantomData,
        };

        Ok((sender, receiver))
    }

    pub fn from_bidirectional<T>(
        sender: Sender,
        receiver: Receiver<T>,
    ) -> Result<Self, SenderReceiverMismatch<T>> {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GroupMessage<T> {
    pub address: GroupAddress,
    pub message: T,
}

impl<T> GroupMessage<T> {
    pub fn from_cemi(cemi: &Cemi) -> Option<Result<GroupMessage<T>, T::ParseError<'_>>>
    where
        T: DataPointType,
    {
        use crate::cemi::*;
        let CemiBody::LData(ldata) = &cemi.body;
        match &ldata.npdu.0 {
            Tpdu::DataGroup(Apdu::GroupValueResponse(data) | Apdu::GroupValueWrite(data)) => {
                Some(match T::from_data(data) {
                    Ok(message) => Ok(GroupMessage {
                        address: ldata.destination.into(),
                        message,
                    }),
                    Err(err) => Err(err),
                })
            }
            _ => None,
        }
    }
}
