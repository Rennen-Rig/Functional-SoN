pub mod graph;
mod gull;
pub mod node;

pub fn main() {
    let g = gull::make_graph();
    println!("{}", g.render2());
}
