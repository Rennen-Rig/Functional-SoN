use std::collections::{HashMap, HashSet};

/// Used to identify nodes. Creating two nodes with the same
/// `NodeID` will cause a panic.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NodeID(u32);
impl NodeID {
    pub const START_NODE_ID: NodeID = NodeID(0);
}

/// Nodes make up a `Graph`, and each `Node` represents a computation of some kind
/// (usually). Nodes (will) have an associated return type, which is computed based on
/// their input types.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Node {
    /// The root for the graph and the only node with no parents.
    /// Used as a parent for `Constant` so it has an ancestor for
    /// graph traversal.
    Start,

    /// End of the program.
    /// TODO: make this use something like Haskell's IO monad to
    /// allow reading + writing data.
    End { input: NodeID },

    /// Remains constant after compilation.
    Constant { value: i32 },

    /// Defines the start of a function.
    FunctionDeclaration {
        /// Links to the node that will be used inside `body` as a placeholder
        /// for the input data
        input: NodeID,
        /// Computes the result of calling the function, in terms of `input`.
        /// If `input` is not used, then the function is a constant function, which
        /// is only usually useful if the function itself is being passed around.
        body: NodeID,
    },

    /// Represents calling a function.
    FunctionApplication {
        /// The function to call, (in future) this may be not be a direct
        /// `FunctionDeclaration` node, but a set of operations that still returns
        /// a function type.
        function: NodeID,
        /// The data to operate on. If multiple arguments are desired, use a tuple.
        input: NodeID,
    },

    /// Creates a tuple of `data.len()` elements.
    ConstructTuple { data: Vec<NodeID> },

    /// Extracts the itme at `at_index` from the input tuple.
    GetTupleElement { from: NodeID, at_index: usize },

    /// Same as `==`.
    Equality { left: NodeID, right: NodeID },

    /// Same as `+`
    Add { left: NodeID, right: NodeID },

    /// Same as `*`
    Multiply { left: NodeID, right: NodeID },

    /// Same as
    /// ```rust
    /// if condition {
    ///     evaluate(on_true)
    /// } else {
    ///     evaluate(on_false)
    /// }
    /// ```
    IfElse {
        /// The boolean to switch on
        condition: NodeID,
        on_true: NodeID,
        on_false: NodeID,
    },
}

impl Node {
    /// Returns the `NodeID`s of all nodes used in this node.
    pub fn get_inputs(&self) -> Vec<NodeID> {
        use Node::*;
        match self {
            Start => vec![],
            End { input } => vec![input.clone()],
            Constant { value: _ } => vec![NodeID::START_NODE_ID],
            FunctionDeclaration { input, body } => vec![input.clone(), body.clone()],
            FunctionApplication { function, input } => vec![function.clone(), input.clone()],
            ConstructTuple { data } => data.clone(),
            GetTupleElement { from, at_index: _ } => vec![from.clone()],
            Equality { left, right } | Add { left, right } | Multiply { left, right } => {
                vec![left.clone(), right.clone()]
            }
            IfElse {
                condition,
                on_true,
                on_false,
            } => vec![condition.clone(), on_true.clone(), on_false.clone()],
        }
    }
}

/// Stores the nodes that have been modified, and so should be checked for peephole
/// optimisations again, inside a stack. If a node is already present in `tracker`
/// then it won't be added to the workgroup.
#[derive(Debug)]
struct Workgroup {
    data: Vec<NodeID>,
    tracker: HashSet<NodeID>,
}

impl Workgroup {
    pub fn new() -> Self {
        Self {
            data: vec![],
            tracker: HashSet::new(),
        }
    }

    pub fn push(&mut self, node_id: NodeID) {
        if !self.tracker.contains(&node_id) {
            self.data.push(node_id);
            self.tracker.insert(node_id);
        }
    }

    pub fn pop(&mut self) -> Option<NodeID> {
        let id = self.data.pop()?;
        assert!(
            self.tracker.remove(&id),
            "Workgroup data contained {:?}, but the tracker did not",
            id
        );
        Some(id)
    }
}

/// A `Graph` represents a program, composed of nodes. The structure of the graph
/// represents inputs for computation. This differs from flow charts, where the
/// structure represents control flow.
///
/// Nodes have some required inputs, and track which other nodes use them as
/// inputs, to allow for modifying the graph faster.
///
/// If any two nodes are identical, they are treated as the same node to reduce
/// redundant computation.
///
/// Graphs will have no cycles, except for around recursive function calls.
#[derive(Debug)]
pub struct Graph {
    /// Stores all the nodes and their outputs in the graph.
    nodes: HashMap<NodeID, (Node, Vec<NodeID>)>,
    /// Used to prevent two identical `Node`s with different ids from existing.
    keys: HashMap<Node, NodeID>,
    /// Stores all updated nodes that still need to be peepholed.
    workgroup: Workgroup,
    /// The previously used id, used so no ids are repeated.
    last_id: u32,
}

impl Graph {
    /// Returns an empty graph.
    pub fn new() -> Self {
        let mut nodes = HashMap::new();
        nodes.insert(NodeID::START_NODE_ID, (Node::Start, vec![]));

        let mut keys = HashMap::new();
        keys.insert(Node::Start, NodeID::START_NODE_ID);

        Self {
            nodes: nodes,
            keys: keys,
            workgroup: Workgroup::new(),
            last_id: NodeID::START_NODE_ID.0,
        }
    }

    pub fn reserve_id(&mut self) -> NodeID {
        self.last_id += 1;
        NodeID(self.last_id)
    }

    fn add_output_to_node(&mut self, used_node: NodeID, input_to_node: NodeID) {
        if let Some((_node, user_nodes)) = self.nodes.get_mut(&used_node) {
            user_nodes.push(input_to_node);
        } else {
            panic!(
                "Node id {:?} was used, but it doesn't exist in the graph",
                used_node
            )
        }
    }

    pub fn insert_node(&mut self, node: Node) -> NodeID {
        if let Some(prev_key) = self.keys.get(&node) {
            prev_key.clone()
        } else {
            let id = self.reserve_id();

            for input_node_id in node.get_inputs() {
                self.add_output_to_node(input_node_id, id);
            }

            self.nodes.insert(id, (node, vec![]));
            self.workgroup.push(id);
            id
        }
    }
}
