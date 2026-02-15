use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::{BuildHasher, DefaultHasher, Hash, Hasher, RandomState},
};

use layout::topo::optimizer::RankOptimizer;

/// Used to identify nodes. Creating two nodes with the same
/// `NodeID` will cause a panic.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NodeID(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PassedData {
    Bool(bool),
    Float(ordered_float::OrderedFloat<f32>),
    Int(i32),
    UInt(u32),
    Tuple(Box<Vec<PassedData>>),
}

impl Display for PassedData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PassedData::Bool(boolean) => write!(f, "{}", boolean),
            PassedData::Float(ordered_float) => write!(f, "{}", ordered_float.0),
            PassedData::Int(int) => write!(f, "{}", int),
            PassedData::UInt(uint) => write!(f, "{}u", uint),
            PassedData::Tuple(tuple) => {
                let mut text = "(".to_string();

                for element in tuple.iter().take(tuple.len() - 1) {
                    text += &(element.to_string() + ", ");
                }

                match tuple.last() {
                    Some(element) => text += element.to_string().as_str(),
                    None => {}
                }
                text += ")";

                write!(f, "{}", text)
            }
        }
    }
}

/// Nodes make up a `Graph`, and each `Node` represents a computation of some kind
/// (usually). Nodes (will) have an associated return type, which is computed based on
/// their input types.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Node {
    /// End of the program.
    /// TODO: make this use something like Haskell's IO monad to
    /// allow reading + writing data.
    /// Also make sure all programs have this
    End { input: NodeID },

    /// Remains constant after compilation.
    Constant { value: PassedData },

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

    /// To be used in the body of `FunctionDeclaration`
    FunctionInput {
        /// The associated function this input is for.
        function: NodeID,
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
    IfThenElse {
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
            End { input } => vec![input.clone()],
            Constant { value: _ } => vec![],

            FunctionDeclaration { input, body } => vec![input.clone(), body.clone()],
            FunctionInput { function } => vec![function.clone()],
            FunctionApplication { function, input } => vec![function.clone(), input.clone()],

            ConstructTuple { data } => data.clone(),
            GetTupleElement { from, at_index: _ } => vec![from.clone()],

            Equality { left, right } | Add { left, right } | Multiply { left, right } => {
                vec![left.clone(), right.clone()]
            }
            IfThenElse {
                condition,
                on_true,
                on_false,
            } => vec![condition.clone(), on_true.clone(), on_false.clone()],
        }
    }

    pub fn create_display_node(&self) -> layout::std_shapes::shapes::Element {
        use Node::*;
        use layout::{
            core::{base::Orientation, geometry::Point, style::StyleAttr},
            std_shapes::shapes::{Element, ShapeKind},
        };

        const ORIENTATION: Orientation = Orientation::TopToBottom;

        match self {
            End { input: _ } => Element::create(
                ShapeKind::new_double_circle("END"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Constant { value } => Element::create(
                ShapeKind::new_box(value.to_string().as_str()),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            FunctionDeclaration { input: _, body: _ } => Element::create(
                ShapeKind::new_box("function"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            FunctionInput { function: _ } => Element::create(
                ShapeKind::new_box("input"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            FunctionApplication {
                function: _,
                input: _,
            } => Element::create(
                ShapeKind::new_circle("|>"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            ConstructTuple { data: _ } => Element::create(
                ShapeKind::new_circle("tuple"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            GetTupleElement { from: _, at_index } => Element::create(
                ShapeKind::new_circle(format!(".{}", at_index).as_str()),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Equality { left: _, right: _ } => Element::create(
                ShapeKind::new_circle("=="),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Add { left: _, right: _ } => Element::create(
                ShapeKind::new_circle("+"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Multiply { left: _, right: _ } => Element::create(
                ShapeKind::new_circle("*"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            IfThenElse {
                condition: _,
                on_true: _,
                on_false: _,
            } => Element::create(
                ShapeKind::new_box("switch"),
                StyleAttr::simple(),
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
        }
    }

    pub fn get_labelled_inputs(&self) -> Vec<(NodeID, String)> {
        use Node::*;
        let unlabelled = "".to_string();

        match self {
            End { input } => vec![(input.clone(), unlabelled)],
            Constant { value: _ } => vec![],

            FunctionDeclaration { input, body } => {
                vec![
                    (input.clone(), "body".to_string()),
                    (body.clone(), "input".to_string()),
                ]
            }
            FunctionInput { function } => vec![(function.clone(), unlabelled)],
            FunctionApplication { function, input } => vec![
                (function.clone(), "apply".to_string()),
                (input.clone(), "to data".to_string()),
            ],

            ConstructTuple { data } => data
                .iter()
                .enumerate()
                .map(|(index, node_id)| (node_id.clone(), index.to_string()))
                .collect(),
            GetTupleElement { from, at_index } => vec![(from.clone(), at_index.to_string())],

            Equality { left, right } | Add { left, right } | Multiply { left, right } => {
                vec![
                    (left.clone(), unlabelled.clone()),
                    (right.clone(), unlabelled),
                ]
            }
            IfThenElse {
                condition,
                on_true,
                on_false,
            } => vec![
                (condition.clone(), "if".to_string()),
                (on_true.clone(), "then".to_string()),
                (on_false.clone(), "else".to_string()),
            ],
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
/// represents inputs for computation.
///
/// This differs from flow charts, where the
/// structure represents control flow.
///
/// - Nodes have some required inputs, and track which other nodes use them as
/// inputs, to allow for modifying the graph faster.
/// - If any two nodes are identical, they are treated as the same node to reduce
/// redundant computation.
/// - Graphs will have no cycles, except for around recursive function calls.
#[derive(Debug)]
pub struct Graph {
    /// Stores all the nodes and their outputs in the graph.
    nodes: HashMap<NodeID, (Node, Vec<NodeID>)>,
    /// Used to prevent two identical `Node`s with different ids from existing.
    keys: HashMap<Node, NodeID>,
    /// Stores all updated nodes that still need to be peepholed.
    workgroup: Workgroup,
    /// The previously used id, used so no ids are repeated.
    next_id: u32,
}

impl Graph {
    /// Returns an empty graph.
    pub fn new() -> Self {
        let nodes = HashMap::new();
        let keys = HashMap::new();

        Self {
            nodes: nodes,
            keys: keys,
            workgroup: Workgroup::new(),
            next_id: 0,
        }
    }

    pub fn reserve_id(&mut self) -> NodeID {
        let this_id = self.next_id;
        self.next_id += 1;
        NodeID(this_id)
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
            self.keys.insert(node.clone(), id);

            for input_node_id in node.get_inputs() {
                self.add_output_to_node(input_node_id, id);
            }

            self.nodes.insert(id, (node, vec![]));
            self.workgroup.push(id);
            id
        }
    }

    pub fn create_image(&self, name: &str) -> Result<(), std::io::Error> {
        use layout::{
            adt::dag::NodeHandle,
            backends::svg::SVGWriter,
            core::{base::Orientation, utils::save_to_file},
            std_shapes::shapes::Arrow,
            topo::layout::VisualGraph,
        };

        let mut vg = VisualGraph::new(Orientation::TopToBottom);

        let mut id_handle_map: HashMap<NodeID, NodeHandle> = HashMap::new();

        for (node_id, (node, _)) in self.nodes.iter() {
            let handle = vg.add_node(node.create_display_node());

            id_handle_map.insert(*node_id, handle);
        }

        for (child_node_id, (child_node, _)) in self.nodes.iter() {
            let child_node_handle = id_handle_map.get(&child_node_id).unwrap();

            for (parent_node_id, label) in child_node.get_labelled_inputs() {
                let parent_node_handle = id_handle_map.get(&parent_node_id).unwrap();

                vg.add_edge(
                    Arrow::simple(label.as_str()),
                    *parent_node_handle,
                    *child_node_handle,
                );
            }
        }

        let mut svg = SVGWriter::new();

        vg.do_it(false, false, false, &mut svg);

        save_to_file(
            format!("generated_graphs/{}.svg", name).as_str(),
            &svg.finalize(),
        )
    }
}
