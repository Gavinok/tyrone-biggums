

use std::fmt::Display;

use futures::{stream::{SplitStream, SplitSink}, StreamExt, SinkExt};

use log::error;
use tokio::{sync::mpsc::{Sender, channel, Receiver}, net::TcpStream};
use tokio_tungstenite::{WebSocketStream, tungstenite};


use crate::error::BoomerError;

use super::message::{Message};

type Tx = Sender<Message>;
type Rx = Receiver<Message>;
type WSStream = WebSocketStream<TcpStream>;
type MessageStream = SplitStream<WSStream>;

pub struct Socket {
    rx: Option<Receiver<Message>>,
    outgoing: SplitSink<WebSocketStream<TcpStream>, tungstenite::Message>
}

pub async fn handle_messages(mut incoming: MessageStream, tx: Tx) {
    while let Some(Ok(msg)) = incoming.next().await {
        if msg.is_text() {
            let text = match msg.into_text() {
                Err(_) => return,
                Ok(txt) => txt,
            };

            if let Ok(msg) = text.try_into() {
                tx.send(msg).await;
            } else {
                error!("unable to deserialize message from socket")
            }
        }
    }
}

impl Socket {
    pub async fn new(ws_stream: WSStream) -> Socket {
        let (outgoing, incoming) = ws_stream.split();

        // from network to me
        let (incoming_tx, incoming_rx) = channel::<Message>(10);

        tokio::spawn(handle_messages(incoming, incoming_tx));

        return Socket {
            rx: Some(incoming_rx),
            outgoing,
        };
    }

    pub async fn push(&mut self, msg: Message) -> Result<(), BoomerError> {
        let msg: tungstenite::Message = tungstenite::Message::Text(msg.try_into()?);
        self.outgoing.send(msg).await;
        return Ok(());
    }

    pub fn get_receiver(&mut self) -> Option<Rx> {
        return std::mem::take(&mut self.rx);
    }
}

impl Display for Socket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Socket here!")
    }
}

