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
pub struct NodeUsesPair<Node: ComputationNode> {
    node: Node,
    uses: Vec<NodeID>,
}

#[derive(Clone, Debug)]
pub struct Graph<Node: ComputationNode> {
    /// Maps from the node's [`NodeID`] to the node itself, and the [`NodeID`]s of
    /// everything that takes it as an input
    nodes_graph: HashMap<NodeID, NodeUsesPair<Node>>,
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

            // Cache that this node has been recognised, so no
            // duplicate nodes are added to the graph
            self.recognised_nodes
                .insert(UniqueNodeWrapper(node.clone()), id);

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

        let mut pet_graph: PetGraph<(), ()> = PetGraph::new();
        let mut index_to_id: HashMap<NodeIndex<u32>, NodeID> = HashMap::new();
        let mut id_to_index: HashMap<NodeID, NodeIndex<u32>> = HashMap::new();

        for (node_id, _node) in self.nodes_graph.iter() {
            let p_index = pet_graph.add_node(());
            index_to_id.insert(p_index, *node_id);
            id_to_index.insert(*node_id, p_index);
        }

        for (source_node_id, NodeUsesPair { node: _, uses }) in self.nodes_graph.iter() {
            let source_index = id_to_index
                .get(source_node_id)
                .expect("Added an edge for a node that isn't in the mapping");

            uses.iter().for_each(|target_node_id| {
                let target_index = id_to_index
                    .get(target_node_id)
                    .expect("Added an edge for a node that isn't in the mapping");

                pet_graph.add_edge(*source_index, *target_index, ());
            });
        }

        use petgraph::{
            dot::{self, Config},
            graph::EdgeReference,
            visit::EdgeRef,
        };

        let get_edge_attrs = |_graph, edge_ref: EdgeReference<'_, ()>| {
            let source_id = index_to_id
                .get(&edge_ref.source())
                .expect("There was an edge from an unmapped node");
            let target_id = index_to_id
                .get(&edge_ref.target())
                .expect("There was an edge to an unmapped node");

            let source_node = &self.nodes_graph.get(source_id).unwrap().node;
            let target_node = &self.nodes_graph.get(target_id).unwrap().node;

            RenderNode::make_edge_attributes(source_node, target_node)
        };

        let get_node_attrs = |_graph, (node_index, ()): (NodeIndex, &())| {
            let node_id = index_to_id
                .get(&node_index)
                .expect("There was a node not tracked in the mapping between graph formats");
            let node = &self.nodes_graph.get(node_id).unwrap().node;

            RenderNode::make_node_attributes(node)
        };

        let graph_dot = dot::Dot::with_attr_getters(
            &pet_graph,
            &[Config::EdgeNoLabel, Config::NodeNoLabel],
            &get_edge_attrs,
            &get_node_attrs,
        );

        format!("{:?}", graph_dot)
    }
}
