use std::collections::HashMap;

use crate::node::{ComputationNode, NodeID, RenderNode};

#[derive(Clone, Debug)]
pub struct NodeUsesPair<Node: ComputationNode> {
    node: Node,
    uses: Vec<NodeID>,
}

#[derive(Clone, Debug)]
pub struct Graph<Node: ComputationNode> {
    /// Maps from the node's [`NodeID`] to the node itself, and the [`NodeID`]s of
    /// everything that takes it as an input
    nodes_graph: HashMap<NodeID, NodeUsesPair<Node>>,
    recognised_nodes: HashMap<Node::ReducedForm, NodeID>,
    used_ids: u32,
}

impl<Node: ComputationNode> Graph<Node> {
    /// Reserves a [`NodeID`] in self, and returns it for use.
    fn reserve_id(&mut self) -> NodeID {
        let id = NodeID(self.used_ids);
        self.used_ids += 1;
        id
    }

    /// Creates a new [`Graph<Node>`].
    pub fn new() -> Self {
        Self {
            nodes_graph: HashMap::new(),
            recognised_nodes: HashMap::new(),
            used_ids: 0,
        }
    }

    pub fn insert_node(&mut self, node: Node) -> NodeID {
        if let Some(id) = self.recognised_nodes.get(&node.reduce()) {
            *id
        } else {
            let id = self.reserve_id();

            // Cache that this node has been recognised, so no
            // duplicate nodes are added to the graph
            self.recognised_nodes.insert(node.reduce(), id);

            // Add this node's id to the `uses` field of all
            // input nodes
            node.get_inputs().iter().for_each(|target_node_id| {
                self.nodes_graph
                    .get_mut(target_node_id)
                    .expect("Tried inserted a node using an unknown node")
                    .uses
                    .push(id);
            });

            self.nodes_graph
                .insert(id, NodeUsesPair { node, uses: vec![] });

            id
        }
    }
}

impl<Node: ComputationNode + RenderNode> Graph<Node> {
    pub fn render(&self) -> String {
        use petgraph::{Graph as PetGraph, graph::NodeIndex};

        let mut pet_graph: PetGraph<String, String> = PetGraph::new();
        let mut id_to_index: HashMap<NodeID, NodeIndex<u32>> = HashMap::new();

        for (node_id, NodeUsesPair { node, uses: _ }) in self.nodes_graph.iter() {
            let p_index = pet_graph.add_node(node.make_node_attributes().trim().to_string());
            id_to_index.insert(*node_id, p_index);
        }

        for (target_node_id, NodeUsesPair { node, uses: _ }) in self.nodes_graph.iter() {
            let target_index = id_to_index
                .get(target_node_id)
                .expect("Added an edge for a node that isn't in the mapping");

            for (source_node_id, edge_attrs) in node.edges_and_attributes() {
                let source_index = id_to_index
                    .get(&source_node_id)
                    .expect("Added an edge for a node that isn't in the mapping");

                pet_graph.add_edge(*source_index, *target_index, edge_attrs.trim().to_string());
            }
        }

        use petgraph::{
            dot::{self, Config},
            graph::EdgeReference,
        };

        let get_edge_attrs =
            |_graph, edge_ref: EdgeReference<'_, String>| edge_ref.weight().clone();

        let get_node_attrs =
            |_graph, (_node_index, attributes): (NodeIndex, &String)| attributes.clone();

        let graph_dot = dot::Dot::with_attr_getters(
            &pet_graph,
            &[Config::EdgeNoLabel, Config::NodeNoLabel],
            &get_edge_attrs,
            &get_node_attrs,
        );

        format!("{:?}", graph_dot)
    }
}
