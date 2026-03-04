//use crate::nodes::{GraphBuilder, Node, PassedData};

pub mod graph;
mod gull;
pub mod node;

pub fn main() {
    // let mut g = Graph::new();
    // let a = g.insert_node(TestNodes::B {
    //     colour: "#ff0000".to_string(),
    // });
    // let _b = g.insert_node(TestNodes::A {
    //     parent: a,
    //     name: "john".to_string(),
    // });
    // let _c = g.insert_node(TestNodes::A {
    //     parent: a,
    //     name: "James".to_string(),
    // });

    // let _d = g.insert_node(TestNodes::B {
    //     colour: "#44dd55".to_string(),
    // });

    // println!("{}", g.render());

    let g = gull::make_graph();
    println!("{}", g.render());
}
