use crate::nodes::{Graph, Node, PassedData};

pub mod nodes;

fn main() {
    let mut graph = Graph::new();
    let n0 = graph.insert_node(Node::Constant {
        value: (PassedData::Float(13.0.into())),
    });

    let n1 = graph.insert_node(Node::Constant {
        value: (PassedData::Float(14.0.into())),
    });

    let n2 = graph.insert_node(Node::Add {
        left: n0,
        right: n1,
    });

    let n3 = graph.insert_node(Node::Constant {
        value: PassedData::Float(13.0.into()),
    });
    let n4 = graph.insert_node(Node::Constant {
        value: PassedData::Bool(true),
    });

    let n5 = graph.insert_node(Node::ConstructTuple {
        data: vec![n2, n4, n3],
    });

    let n6 = graph.insert_node(Node::GetTupleElement {
        from: n5,
        at_index: 0,
    });

    let _return_node = graph.insert_node(Node::End { input: n6 });

    graph.create_image("example").unwrap();
}
