use std::{ops::AddAssign, sync::Mutex};

pub mod nodes;

fn main() {
    let _ = nodes::NodeID::START_NODE_ID;

    println!("Hello, world!");
}
