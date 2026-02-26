use petgraph::{
    Directed, dot,
    matrix_graph::{self, NodeIndex},
};
use std::collections::HashMap;

use crate::node::{ComputationNode, NodeID, RenderNode};

#[derive(Debug, Clone)]
struct UniqueNodeWrapper<Node: ComputationNode>(Node);

impl<Node: ComputationNode> PartialEq for UniqueNodeWrapper<Node> {
    fn eq(&self, other: &Self) -> bool {
        self.0.node_eq(&other.0)
    }
}

impl<Node: ComputationNode> Eq for UniqueNodeWrapper<Node> {}

impl<Node: ComputationNode> std::hash::Hash for UniqueNodeWrapper<Node> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.node_hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct Graph<Node: ComputationNode> {
    nodes_graph: HashMap<NodeID, Node>,
    recognised_nodes: HashMap<UniqueNodeWrapper<Node>, NodeID>,
    used_ids: u32,
}

impl<Node: ComputationNode> Graph<Node> {
    /// Reserves a [`NodeID`] in self, and returns it for use.
    fn reserve_id(&mut self) -> NodeID {
        let id = NodeID(self.used_ids);
        self.used_ids += 1;
        id
    }

    /// Creates a new [`Graph<N>`].
    pub fn new() -> Self {
        Self {
            nodes_graph: HashMap::new(),
            recognised_nodes: HashMap::new(),
            used_ids: 0,
        }
    }

    pub fn insert_node(&mut self, node: Node) -> NodeID {
        if let Some(id) = self.recognised_nodes.get(&UniqueNodeWrapper(node.clone())) {
            *id
        } else {
            let id = self.reserve_id();
            self.recognised_nodes
                .insert(UniqueNodeWrapper(node.clone()), id);

            self.nodes_graph.insert(id, node);

            id
        }
    }
}

impl<Node: ComputationNode + RenderNode> Graph<Node> {
    pub fn render(&self) {
        use petgraph::Graph as PetGraph;

        let mut pet_graph: PetGraph<Node, ()> = PetGraph::new();
        let mut mapping: HashMap<NodeID, NodeIndex<u32>> = HashMap::new();

        for (node_id, node) in self.nodes_graph.iter() {
            let p_index = pet_graph.add_node(node.clone());
            mapping.insert(*node_id, p_index);
        }

        for node_id in self.nodes_graph.

        use petgraph::dot::Dot;
    }
}
