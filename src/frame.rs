use crate::{cri::ConnectRequest, header::Header};

pub enum Body {
    ConnectRequest(ConnectRequest),
}

pub struct Frame {
    pub header: Header,
    pub body: Body,
}

impl Frame {

    pub fn new(header: Header, body: Body) -> Self {
        Self { header, body }
    }

    pub fn serialize(&self) -> Vec<u8> {
        // let mut result = self.header.serialize();
        vec![]
    }

    /// Set the frame's body.
    pub fn set_body(&mut self, body: Body) {
        // self.header.set_body_len(body.len());
        self.body = body;
    }
}