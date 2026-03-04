use crate::{
    graph::Graph,
    node::{ComputationNode, NodeID, RenderNode},
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Node {
    Constant(i32),
    Add(NodeID, NodeID),
    GreaterThan(NodeID, NodeID),
}

impl ComputationNode for Node {
    type ReducedForm = Node;

    fn get_inputs(&self) -> Vec<NodeID> {
        use Node::*;

        match self {
            Constant(_) => vec![],
            Add(a, b) | GreaterThan(a, b) => vec![*a, *b],
        }
    }

    fn reduce(&self) -> Self::ReducedForm {
        self.clone()
    }
}

impl RenderNode for Node {
    fn get_setup() {}

    fn make_edge_attributes(from: &Self, _to: &Self) -> String {
        match from {
            Node::Constant(_) | Node::Add(_, _) => "".to_string(),
            Node::GreaterThan(_node_id, _node_id1) => "".to_string(),
        }
    }

    fn make_node_attributes(node: &Self) -> String {
        match node {
            Node::Constant(v) => format!(
                r##"
                    "label" = "{v}",
                    "shape" = "box",
                    "color" = "#9212b4"
                "##,
            ),
            Node::Add(_, _) => r##"
                "label" = "+",
                "shape" = "circle",
                "color" = "#44aa55"
            "##
            .to_string(),
            Node::GreaterThan(_, _) => r##"
                "label" = ">",
                "shape" = "Msquare",
                "color" = "#ffaa11"
            "##
            .to_string(),
        }
    }
}

pub fn make_graph() -> Graph<Node> {
    let mut g = Graph::new();

    let a = g.insert_node(Node::Constant(414));
    let b = g.insert_node(Node::Constant(15));
    let c = g.insert_node(Node::Constant(149));
    let d = g.insert_node(Node::Constant(3));
    let e = g.insert_node(Node::Constant(149));

    let p = g.insert_node(Node::Add(a, b));
    let q = g.insert_node(Node::Add(p, c));
    let r = g.insert_node(Node::Add(p, q));
    let s = g.insert_node(Node::Add(d, e));
    let _t = g.insert_node(Node::GreaterThan(r, s));

    g
}
