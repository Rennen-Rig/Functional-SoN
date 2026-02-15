use crate::nodes::{Graph, Node};

pub mod nodes;

fn main() {
    let mut graph = Graph::new();
    let n0 = graph.insert_node(Node::Constant {
        value: (nodes::PassedData::Float(13.0.into())),
    });

    let n1 = graph.insert_node(Node::Constant {
        value: (nodes::PassedData::Float(14.0.into())),
    });

    let n2 = graph.insert_node(Node::Add {
        left: n0,
        right: n1,
    });

    let _n3 = graph.insert_node(Node::End { input: n2 });

    graph.create_image("example").unwrap();
}
