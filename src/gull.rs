use crate::{
    edge_attributes,
    graph::{
        Graph,
        render::{self, Attributes},
    },
    node::{ComputationNode, NodeID, RenderNode},
    node_attributes,
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Node {
    Constant(i32),
    Add(NodeID, NodeID),
    GreaterThan(NodeID, NodeID),
    FunctionApplication { input: NodeID, function: NodeID },
}

impl ComputationNode for Node {
    type ReducedForm = Node;

    fn get_inputs(&self) -> Vec<NodeID> {
        use Node::*;

        match self {
            Constant(_) => vec![],
            Add(a, b)
            | GreaterThan(a, b)
            | FunctionApplication {
                input: a,
                function: b,
            } => vec![*a, *b],
        }
    }

    fn reduce(&self) -> Self::ReducedForm {
        self.clone()
    }
}

impl RenderNode for Node {
    fn make_node_attributes(&self) -> Attributes {
        match self {
            Node::Constant(v) => {
                node_attributes![
                    "label" => format!("\"{v}\"")
                    "shape" => "box"
                    "color" => "#9212b4"
                ]
            }
            Node::Add(_, _) => node_attributes![
                "label" => r#""+""#
                "shape" => "circle"
                "color" => "#44aa55"
            ],
            Node::GreaterThan(_, _) => node_attributes![
                "label" => r#"">""#
                "shape" => "Msquare"
                "color" => "#ffaa11"
            ],
            Node::FunctionApplication {
                input: _,
                function: _,
            } => node_attributes!(
                "label" => "call"
                "shape" => "diamond"
                "color" => "#52bc13"
            ),
        }
    }

    fn edges_and_attributes(&self) ->  {
        match self {
            Node::GreaterThan(left, right) => edge_attributes!(
                left, [
                    "label" => "left"
                ];
                right, [
                    "label" => "right"
                ];
            ),

            Node::FunctionApplication { input, function } => edge_attributes!(
                input, [
                    "label" => "to"
                    "style" => "dotted"
                    "color" => "#222222"
                ];
                function, [
                    "label" => "apply"
                    "style" => "dashed"
                    "color" => "#000000"
                ];
            ),

            _ => self
                .get_inputs()
                .iter()
                .map(|n| (*n, edge_attributes!()))
                .collect(),
        }
    }

    fn graph_labels() -> Vec<String> {
        vec![]
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
