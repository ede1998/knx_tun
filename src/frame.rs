use crate::{cri::ConnectRequest, header::Header};

pub enum Body {
    ConnectRequest(ConnectRequest),
}

impl Body {
    pub fn serialize(&self) -> Vec<u8> {
        match self {
            Body::ConnectRequest(req) => req.serialize(),
        }
    }

    pub fn len(&self) -> u16 {
        match self {
            Body::ConnectRequest(_) => ConnectRequest::len() as u16,
        }
    }
}

pub struct Frame {
    header: Header,
    body: Body,
}

impl Frame {

    pub fn new(header: Header, body: Body) -> Self {
        Self { header, body }
    }

    pub fn serialize(&self) -> Vec<u8> {
        // let mut result = self.header.serialize();
        let mut result = vec![];
        result.extend_from_slice(&self.body.serialize());
        result
    }

    /// Set the frame's body.
    pub fn set_body(&mut self, body: Body) {
        // self.header.set_body_len(body.len());
        self.body = body;
    }
}