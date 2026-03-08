use std::hash::Hash;

use crate::graph::{Edge, render};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeID(pub(crate) u32);

/// A trait that must be implemented by
pub trait ComputationNode: Clone {
    /// Used to compare nodes to see if they will have identical behaivour.
    /// Fields such as labels that do not impact what a node will do in code
    /// should be removed from this.
    type ReducedForm: Eq + Hash;

    /// Converts ot reduced form
    fn reduce(&self) -> Self::ReducedForm;

    /// Used to track all the nodes this node uses to compute.
    fn get_inputs(&self) -> Vec<NodeID>;
}

pub trait RenderNode {
    fn graph_labels() -> Vec<String>;

    fn edges_and_attributes(&self) -> Vec<render::ArgumentAttributes>;

    fn make_node_attributes(&self) -> render::Attributes;
}
