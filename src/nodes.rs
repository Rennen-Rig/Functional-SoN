use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NodeID(u32);
impl NodeID {
    pub const START_NODE_ID: NodeID = NodeID(0);
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Node {
    Start,
    End {
        input: NodeID,
    },
    Constant {
        value: i32,
    },

    FunctionDeclaration {
        input: NodeID,
        body: NodeID,
    },
    Call {
        function: NodeID,
        data: NodeID,
    },

    ConstructTuple {
        data: Vec<NodeID>,
    },
    GetTupleElement {
        from: NodeID,
        get: usize,
    },

    Equality {
        left: NodeID,
        right: NodeID,
    },
    Add {
        left: NodeID,
        right: NodeID,
    },
    Multiply {
        left: NodeID,
        right: NodeID,
    },

    IfElse {
        condition: NodeID,
        on_true: NodeID,
        on_false: NodeID,
    },
}

impl Node {
    pub fn get_inputs(&self) -> Vec<NodeID> {
        use Node::*;
        match self {
            Start => vec![],
            End { input } => vec![input.clone()],
            Constant { value: _ } => vec![NodeID::START_NODE_ID],
            FunctionDeclaration { input, body } => vec![input.clone(), body.clone()],
            Call { function, data } => vec![function.clone(), data.clone()],
            ConstructTuple { data } => data.clone(),
            GetTupleElement { from, get: _ } => vec![from.clone()],
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

struct Workgroup {
    data: Vec<NodeID>,
    tracker: HashSet<NodeID>,
}

pub struct Graph {
    nodes: HashMap<NodeID, (Node, Vec<NodeID>)>,
    keys: HashMap<Node, NodeID>,
    workgroup: Workgroup,
    last_id: u32,
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

impl Graph {
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

    fn add_input_to_node(&mut self, to_node: NodeID, add_input: NodeID) {
        if let Some((_node, outputs)) = self.nodes.get_mut(&to_node) {
            outputs.push(add_input);
        } else {
            panic!(
                "Node id {:?} was used, but it doesn't exist in the graph",
                to_node
            )
        }
    }

    pub fn insert_node(&mut self, node: Node) -> NodeID {
        if let Some(prev_key) = self.keys.get(&node) {
            prev_key.clone()
        } else {
            let id = self.reserve_id();

            for input_node_id in node.get_inputs() {
                self.add_input_to_node(input_node_id, id);
            }

            self.nodes.insert(id, (node, vec![]));
            id
        }
    }
}
