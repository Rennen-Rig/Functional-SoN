use crate::nodes::Graph;

pub mod nodes;
pub mod visuals;

fn main() {
    let graph = Graph::new();

    graph.create_image("awesome_demo_graph").unwrap();
}
