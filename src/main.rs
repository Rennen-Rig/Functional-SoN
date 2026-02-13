use std::{ops::AddAssign, sync::Mutex};

pub mod nodes;

fn main() {
    let mut graph = nodes::Graph::new();

    println!("{:?}", graph);

    let c = graph.insert_node(nodes::Node::Constant {
        value: nodes::PassedData::Int(2),
    });
    println!("{:?}", graph);
    let ret = graph.insert_node(nodes::Node::End { input: c });

    println!("{:?}", graph);

    println!("Hello, world!");
}
