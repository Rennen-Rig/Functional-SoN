use crate::nodes::{GraphBuilder, Node, PassedData};

pub mod nodes;

fn main() {
    // tail recursive factorial
    {
        let mut g = GraphBuilder::new();

        let minus_1 = g.insert_node(Node::Constant {
            value: PassedData::Int(-1),
        });
        let zero = g.insert_node(Node::Constant {
            value: PassedData::Int(0),
        });
        let one = g.insert_node(Node::Constant {
            value: PassedData::Int(1),
        });

        let fact_recursive = g.start_function();

        let n = g.insert_node(Node::GetTupleElement {
            from: fact_recursive.use_input(),
            at_index: 0,
        });
        let acc = g.insert_node(Node::GetTupleElement {
            from: fact_recursive.use_input(),
            at_index: 1,
        });

        let product = g.insert_node(Node::Multiply {
            left: n,
            right: acc,
        });
        let n_pred = g.insert_node(Node::Add {
            left: n,
            right: minus_1,
        });

        let next_call_input = g.insert_node(Node::ConstructTuple {
            data: vec![n_pred, product],
        });
        let next_call = g.insert_node(Node::FunctionApplication {
            function: fact_recursive.use_function(),
            input: next_call_input,
        });

        let comparison = g.insert_node(Node::Equality {
            left: n,
            right: zero,
        });

        let switch = g.insert_node(Node::IfThenElse {
            condition: comparison,
            on_true: one,
            on_false: next_call,
        });

        let fact_recursive = g.end_function(fact_recursive, switch);

        let fact = g.start_function();
        let fact_rec_input = g.insert_node(Node::ConstructTuple {
            data: vec![fact.use_input(), one],
        });
        let fact_rec_call = g.insert_node(Node::FunctionApplication {
            function: fact_recursive,
            input: fact_rec_input,
        });
        let fact = g.end_function(fact, fact_rec_call);

        let seven = g.insert_node(Node::Constant {
            value: PassedData::Int(7),
        });

        let fact_call = g.insert_node(Node::FunctionApplication {
            function: fact,
            input: seven,
        });
        let graph = g.finalise(fact_call);
        graph.create_image("example").unwrap();
    }

    // Non tail recursive version
    {
        let mut g = GraphBuilder::new();

        let fact = g.start_function();
        let one = g.insert_node(Node::Constant {
            value: PassedData::Int(1),
        });
        let zero = g.insert_node(Node::Constant {
            value: PassedData::Int(0),
        });
        let minus_one = g.insert_node(Node::Constant {
            value: PassedData::Int(-1),
        });
        let n_pred = g.insert_node(Node::Add {
            left: fact.use_input(),
            right: minus_one,
        });
        let next_call = g.insert_node(Node::FunctionApplication {
            function: fact.use_function(),
            input: n_pred,
        });

        let product = g.insert_node(Node::Multiply {
            left: fact.use_input(),
            right: next_call,
        });

        let is_zero = g.insert_node(Node::Equality {
            left: fact.use_input(),
            right: zero,
        });

        let switch = g.insert_node(Node::IfThenElse {
            condition: is_zero,
            on_true: one,
            on_false: product,
        });
        let fact = g.end_function(fact, switch);
        let input = g.insert_node(Node::Constant {
            value: PassedData::Int(7),
        });
        let call = g.insert_node(Node::FunctionApplication {
            function: fact,
            input,
        });
        let fact_non_tail = g.finalise(call);
        fact_non_tail.create_image("example2").unwrap();
    }
}
