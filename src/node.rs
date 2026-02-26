#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeID(pub(crate) u32);

/// A trait that must be implemented by
pub trait ComputationNode: Clone {
    /// Used to track all the nodes this node uses to compute.
    fn get_inputs(&self) -> Vec<NodeID>;

    /// Used to indentify nodes that are identical when debug states, or
    /// names used for graph rendering are ignored.
    ///
    ///
    /// This function must follow the same rules as `Eq`.
    fn node_eq(&self, other: &Self) -> bool;

    fn node_hash<H: std::hash::Hasher>(&self, state: &mut H);
}

pub trait RenderNode {
    fn get_setup();
}
