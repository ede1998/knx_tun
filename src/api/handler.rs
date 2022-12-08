use std::collections::HashMap;

use thiserror::Error;

use crate::{
    address::{Address, GroupAddress},
    api::GroupMessage,
    cemi::{Cemi, GroupData},
    dpt::general::{DataPointId, DataPointType},
};

type HandlerCallback<'a> =
    Box<dyn FnMut(&GroupData, GroupAddress) -> Result<bool, (DataPointId, String)> + 'a>;

#[derive(Debug, Error)]
pub enum HandlerError {
    #[error("Expected group address, got: {0}")]
    WrongAdressType(Address),
    #[error("Cemi does not contain group data")]
    MissingGroupData,
    #[error("Failed to interpret the data as {0}: {1}")]
    ConversionError(DataPointId, String),
    #[error("No handler configured for group address {0}")]
    MissingHandler(GroupAddress),
    #[error("The configured handler failed to handle the message")]
    NotHandled,
}

#[derive(Default)]
pub struct Handlers<'a> {
    handlers: HashMap<GroupAddress, HandlerCallback<'a>>,
}

impl<'a> Handlers<'a> {
    pub fn register<T, F>(&mut self, address: GroupAddress, mut f: F)
    where
        F: FnMut(GroupMessage<T>) -> bool + 'a,
        T: DataPointType + 'static,
        for<'b> T::ParseError<'b>: ToString,
    {
        self.handlers.insert(
            address,
            Box::new(move |group_data, addr| {
                let msg = T::from_data(group_data).map_err(|e| (T::ID, e.to_string()))?;
                Ok(f(GroupMessage {
                    address: addr,
                    message: msg,
                }))
            }),
        );
    }

    pub fn register_many<I, T, F>(&mut self, addresses: I, f: F)
    where
        I: IntoIterator<Item = GroupAddress>,
        F: FnMut(GroupMessage<T>) -> bool + Clone + 'a,
        T: DataPointType + 'static,
        for<'b> T::ParseError<'b>: ToString,
    {
        for address in addresses {
            self.register(address, f.clone());
        }
    }

    pub fn register_many_receiver_agnostic<I, T, F>(&mut self, addresses: I, f: F)
    where
        I: IntoIterator<Item = GroupAddress>,
        F: FnMut(T) -> bool + Clone + 'a,
        T: DataPointType + 'static,
        for<'b> T::ParseError<'b>: ToString,
    {
        for address in addresses {
            self.register_receiver_agnostic(address, f.clone());
        }
    }

    pub fn register_receiver_agnostic<T, F>(&mut self, address: GroupAddress, mut f: F)
    where
        F: FnMut(T) -> bool + 'a,
        T: DataPointType + 'static,
        for<'b> T::ParseError<'b>: ToString,
    {
        self.handlers.insert(
            address,
            Box::new(move |group_data, _| {
                let msg = T::from_data(group_data).map_err(|e| (T::ID, e.to_string()))?;
                Ok(f(msg))
            }),
        );
    }

    pub fn handle(&mut self, address: GroupAddress, data: &GroupData) -> Result<(), HandlerError> {
        let handler = self
            .handlers
            .get_mut(&address)
            .ok_or(HandlerError::MissingHandler(address))?;
        match handler(data, address) {
            Err((id, inner)) => Err(HandlerError::ConversionError(id, inner)),
            Ok(true) => Ok(()),
            Ok(false) => Err(HandlerError::NotHandled),
        }
    }

    pub fn handle_raw(&mut self, cemi: &Cemi) -> Result<(), HandlerError> {
        let address = cemi
            .address()
            .try_into()
            .map_err(HandlerError::WrongAdressType)?;
        let data = cemi.group_data().ok_or(HandlerError::MissingGroupData)?;

        self.handle(address, data)
    }
}
