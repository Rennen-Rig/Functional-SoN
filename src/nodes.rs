use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::{BuildHasher, DefaultHasher, Hash, Hasher, RandomState},
    marker::PhantomData,
};

/// Used to identify nodes. Creating two nodes with the same
/// `InternalNodeID` will cause a panic.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct InternalNodeID(u32);

impl InternalNodeID {
    const END: Self = Self(0);

    fn replace(&self, replace: InternalNodeID, with: InternalNodeID) -> Self {
        if *self == replace { with } else { self.clone() }
    }
}

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
    End { input: InternalNodeID },

    /// Remains constant after compilation.
    Constant { value: PassedData },

    /// Defines the start of a function.
    FunctionDeclaration {
        /// Links to the node that will be used inside `body` as a placeholder
        /// for the input data
        input: InternalNodeID,
        /// Computes the result of calling the function, in terms of `input`.
        /// If `input` is not used, then the function is a constant function, which
        /// is only usually useful if the function itself is being passed around.
        body: InternalNodeID,
    },

    /// To be used in the body of `FunctionDeclaration`
    FunctionInput {
        /// The associated function this input is for.
        function: InternalNodeID,
    },

    /// Represents calling a function.
    FunctionApplication {
        /// The function to call, (in future) this may be not be a direct
        /// `FunctionDeclaration` node, but a set of operations that still returns
        /// a function type.
        function: InternalNodeID,
        /// The data to operate on. If multiple arguments are desired, use a tuple.
        input: InternalNodeID,
    },

    /// Creates a tuple of `data.len()` elements.
    ConstructTuple { data: Vec<InternalNodeID> },

    /// Extracts the itme at `at_index` from the input tuple.
    GetTupleElement {
        from: InternalNodeID,
        at_index: usize,
    },

    /// Same as `==`.
    Equality {
        left: InternalNodeID,
        right: InternalNodeID,
    },

    /// Same as `+`
    Add {
        left: InternalNodeID,
        right: InternalNodeID,
    },

    /// Same as `*`
    Multiply {
        left: InternalNodeID,
        right: InternalNodeID,
    },

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
        condition: InternalNodeID,
        on_true: InternalNodeID,
        on_false: InternalNodeID,
    },
}

impl Node {
    /// Returns the `InternalNodeID`s of all nodes used in this node.
    pub fn get_inputs(&self) -> Vec<InternalNodeID> {
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
            core::{base::Orientation, color::Color, geometry::Point, style::StyleAttr},
            std_shapes::shapes::{Element, ShapeKind},
        };

        const ORIENTATION: Orientation = Orientation::TopToBottom;

        let blue_style = StyleAttr {
            line_color: Color::new(0x00b0beff),
            line_width: 2,
            fill_color: Some(Color::new(0x8fd7d7ff)),
            rounded: 0,
            font_size: 15,
        };

        let pink_style = StyleAttr {
            line_color: Color::new(0xf45f74ff),
            line_width: 2,
            fill_color: Some(Color::new(0xff8ca1ff)),
            rounded: 0,
            font_size: 15,
        };
        let green_style = StyleAttr {
            line_color: Color::new(0x98c127ff),
            line_width: 2,
            fill_color: Some(Color::new(0xbdd373ff)),
            rounded: 0,
            font_size: 15,
        };

        let yellow_style = StyleAttr {
            line_color: Color::new(0xffb255ff),
            line_width: 2,
            fill_color: Some(Color::new(0xffcd8eff)),
            rounded: 0,
            font_size: 15,
        };

        match self {
            End { input: _ } => Element::create(
                ShapeKind::new_double_circle("END"),
                pink_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Constant { value } => Element::create(
                ShapeKind::new_box(value.to_string().as_str()),
                yellow_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            FunctionDeclaration { input: _, body: _ } => Element::create(
                ShapeKind::new_box("function"),
                pink_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            FunctionInput { function: _ } => Element::create(
                ShapeKind::new_box("input"),
                pink_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            FunctionApplication {
                function: _,
                input: _,
            } => Element::create(
                ShapeKind::new_circle("|>"),
                green_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            ConstructTuple { data: _ } => Element::create(
                ShapeKind::new_circle("tuple"),
                blue_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            GetTupleElement { from: _, at_index } => Element::create(
                ShapeKind::new_circle(format!(".{}", at_index).as_str()),
                blue_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Equality { left: _, right: _ } => Element::create(
                ShapeKind::new_circle("=="),
                blue_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Add { left: _, right: _ } => Element::create(
                ShapeKind::new_circle("+"),
                blue_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            Multiply { left: _, right: _ } => Element::create(
                ShapeKind::new_circle("*"),
                blue_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
            IfThenElse {
                condition: _,
                on_true: _,
                on_false: _,
            } => Element::create(
                ShapeKind::new_box("switch"),
                pink_style,
                ORIENTATION,
                Point::new(50.0, 50.0),
            ),
        }
    }

    pub fn get_labelled_inputs(&self) -> Vec<(InternalNodeID, String)> {
        use Node::*;
        let unlabelled = "".to_string();

        match self {
            End { input } => vec![(input.clone(), unlabelled)],
            Constant { value: _ } => vec![],

            FunctionDeclaration { input, body } => {
                vec![
                    (input.clone(), "input".to_string()),
                    (body.clone(), "body".to_string()),
                ]
            }
            FunctionInput { function } => vec![(function.clone(), unlabelled)],
            FunctionApplication { function, input } => vec![
                (function.clone(), "apply".to_string()),
                (input.clone(), "to".to_string()),
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

    //TODO: swap this for a map node_id, or remove it
    pub fn replace_uses(&self, replace: InternalNodeID, with: InternalNodeID) -> Self {
        use Node::*;

        match self {
            End { input } => End {
                input: input.replace(replace, with),
            },
            Constant { value: _ } => self.clone(),
            FunctionDeclaration { input, body } => FunctionDeclaration {
                input: input.replace(replace, with),
                body: body.replace(replace, with),
            },
            FunctionInput { function } => FunctionInput {
                function: function.replace(replace, with),
            },
            FunctionApplication { function, input } => FunctionApplication {
                function: function.replace(replace, with),
                input: input.replace(replace, with),
            },
            ConstructTuple { data } => ConstructTuple {
                data: data
                    .iter()
                    .map(|element| element.replace(replace, with))
                    .collect(),
            },
            GetTupleElement { from, at_index } => GetTupleElement {
                from: from.replace(replace, with),
                at_index: at_index.clone(),
            },
            Equality { left, right } => Equality {
                left: left.replace(replace, with),
                right: right.replace(replace, with),
            },
            Add { left, right } => Add {
                left: left.replace(replace, with),
                right: right.replace(replace, with),
            },
            Multiply { left, right } => Multiply {
                left: left.replace(replace, with),
                right: right.replace(replace, with),
            },
            IfThenElse {
                condition,
                on_true,
                on_false,
            } => IfThenElse {
                condition: condition.replace(replace, with),
                on_true: on_true.replace(replace, with),
                on_false: on_false.replace(replace, with),
            },
        }
    }
}

/// Stores the nodes that have been modified, and so should be checked for peephole
/// optimisations again, inside a stack. If a node is already present in `tracker`
/// then it won't be added to the workgroup.
#[derive(Debug)]
struct Workgroup {
    data: Vec<InternalNodeID>,
    tracker: HashSet<InternalNodeID>,
}

impl Workgroup {
    pub fn new() -> Self {
        Self {
            data: vec![],
            tracker: HashSet::new(),
        }
    }

    pub fn push(&mut self, node_id: InternalNodeID) {
        if !self.tracker.contains(&node_id) {
            self.data.push(node_id);
            self.tracker.insert(node_id);
        }
    }

    pub fn pop(&mut self) -> Option<InternalNodeID> {
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
    nodes: HashMap<InternalNodeID, (Node, Vec<InternalNodeID>)>,
    /// Used to prevent two identical `Node`s with different ids from existing.
    keys: HashMap<Node, InternalNodeID>,
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

    pub fn reserve_id(&mut self) -> InternalNodeID {
        let this_id = self.next_id;
        self.next_id += 1;
        InternalNodeID(this_id)
    }

    fn add_output_to_node(&mut self, used_node: InternalNodeID, input_to_node: InternalNodeID) {
        if let Some((_node, user_nodes)) = self.nodes.get_mut(&used_node) {
            user_nodes.push(input_to_node);
        } else {
            panic!(
                "Node id {:?} was used, but it doesn't exist in the graph",
                used_node
            )
        }
    }

    pub fn insert_node(&mut self, node: Node) -> InternalNodeID {
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

        let mut id_handle_map: HashMap<InternalNodeID, NodeHandle> = HashMap::new();

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

#[derive(PartialEq, Eq, Hash)]
pub struct FunctionID {
    declaration_id: InternalNodeID,
    input_id: InternalNodeID,
}

impl FunctionID {
    pub fn use_input(&self) -> InternalNodeID {
        self.input_id
    }
    pub fn use_function(&self) -> InternalNodeID {
        self.declaration_id
    }
}

/// Type checked wrapper for `InternalNodeID` during graph creation
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeID<T> {
    node_id: InternalNodeID,
    phantom_data: PhantomData<T>,
}

pub mod construct_node {
    use crate::nodes::{Node, NodeID};

    pub fn add<T>(left: NodeID<T>) -> Node {
        //Node::Add { left: (), right: () }
        todo!()
    }
}

pub struct GraphBuilder {
    nodes: HashMap<InternalNodeID, Node>,
    recognised_nodes: HashMap<Node, InternalNodeID>,
    next_id: u32,
    functions_to_be_added: HashSet<FunctionID>,
}

impl GraphBuilder {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            recognised_nodes: HashMap::new(),
            next_id: 1,
            functions_to_be_added: HashSet::new(),
        }
    }

    fn reserve_id(&mut self) -> InternalNodeID {
        let id = self.next_id;
        self.next_id += 1;
        InternalNodeID(id)
    }

    pub fn start_function(&mut self) -> FunctionID {
        let declaration_id = self.reserve_id();
        let input_id = self.reserve_id();

        self.functions_to_be_added.insert(FunctionID {
            declaration_id,
            input_id,
        });

        FunctionID {
            declaration_id,
            input_id,
        }
    }

    pub fn insert_node(&mut self, node: Node) -> InternalNodeID {
        if let Some(id) = self.recognised_nodes.get(&node) {
            *id
        } else {
            let node_id = self.reserve_id();
            self.nodes.insert(node_id, node);
            node_id
        }
    }

    pub fn end_function(&mut self, function: FunctionID, body: InternalNodeID) -> InternalNodeID {
        assert!(
            self.functions_to_be_added.remove(&function),
            "Tried defining the same function twice"
        );

        self.functions_to_be_added.remove(&function);

        self.nodes.insert(
            function.input_id,
            Node::FunctionInput {
                function: function.declaration_id,
            },
        );
        self.nodes.insert(
            function.declaration_id,
            Node::FunctionDeclaration {
                input: function.input_id,
                body,
            },
        );

        function.declaration_id
    }

    pub fn finalise(mut self, return_node: InternalNodeID) -> Graph {
        assert!(self.functions_to_be_added.is_empty());

        self.nodes
            .insert(InternalNodeID::END, Node::End { input: return_node });

        let mut use_tracked_nodes: HashMap<InternalNodeID, (Node, Vec<InternalNodeID>)> = self
            .nodes
            .iter()
            .map(|(node_id, node)| (*node_id, (node.clone(), vec![])))
            .collect();

        for (node_id, node) in self.nodes {
            for input_node_id in node.get_inputs() {
                use_tracked_nodes
                    .get_mut(&input_node_id)
                    .unwrap()
                    .1
                    .push(node_id);
            }
        }

        Graph {
            nodes: use_tracked_nodes,
            keys: self.recognised_nodes,
            workgroup: Workgroup::new(),
            next_id: self.next_id,
        }
    }
}
