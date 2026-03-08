use std::fmt::Display;

use crate::{graph::Edge, node::NodeID};

/// Represents a single attribute
pub struct Attribute {
    pub(crate) name: String,
    pub(crate) value: String,
}

impl Attribute {
    pub fn new(name: String, value: String) -> Self {
        Self { name, value }
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}={}", self.name, self.value)
    }
}

pub struct Attributes {
    pub(crate) attributes: Vec<Attribute>,
}

impl Attributes {
    pub fn new(attributes: Vec<Attribute>) -> Self {
        Self { attributes }
    }
}

impl Display for Attributes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.attributes
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

pub struct ArgumentAttributes {
    pub(crate) argument: NodeID,
    pub(crate) attributes: Attributes,
}

pub struct EdgeAttributes {
    pub(crate) edge: Edge,
    pub(crate) attributes: Attributes,
}

impl Display for EdgeAttributes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.edge, self.attributes)
    }
}

/// Allows writing attributes using pattern matching.
#[macro_export]
macro_rules! node_attributes {
    [$($name:expr => $val:expr)*] => {
        crate::graph::render::Attributes {
            attributes: vec![$(
            Attribute {
                name: std::convert::Into::<String>::into($name),
                value: std::convert::Into::<String>::into($val)
            },
        )*] }
    };
}

/// Used to create [`InputAttributes`] from the input nodes and attribute parameters
///
/// ## Example use
///
/// ```rust
///
/// // in `impl RenderNode for Node`
/// fn edges_and_attributes(&self) -> Vec<(NodeID, render::Attribs)> {
///     match self {
///         Node::FunctionApplication { input, function } => edge_attributes!(
///             input, [
///                 "label" => "to"
///                 "style" => "dotted"
///                 "color" => "#222222"
///             ];
///             function, [
///                 "label" => "apply"
///                 "style" => "dashed"
///                 "color" => "#000000"
///             ];
///         ),
///         _ => todo!()
///     }      
/// }
/// ```
///
#[macro_export]
macro_rules! edge_attributes {
    [$($node_id:expr, [ $($name:expr => $val:expr)* ];)*] => {
        vec![
            $((*$node_id,
                vec![$(
                    (
                        std::convert::Into::<String>::into($name),
                        std::convert::Into::<String>::into($val)
                    ),
                )*]
            ),)*
        ]
    };
}
