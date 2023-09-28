use cel_interpreter::{Context, Program};
use std::thread::scope;

fn main() {
    let program = Program::compile("a + b").unwrap();

    scope(|scope| {
        let mut handles = vec![];
        handles.push(scope.spawn(|| {
            let mut context = Context::default();
            context.add_variable("a", 1);
            context.add_variable("b", 2);
            let value = program.execute(&context).unwrap();
            assert_eq!(value, 3.into());
        }));
        handles.push(scope.spawn(|| {
            let mut context = Context::default();
            context.add_variable("a", 2);
            context.add_variable("b", 4);
            let value = program.execute(&context).unwrap();
            assert_eq!(value, 6.into());
        }));
        for handle in handles {
            handle.join().unwrap();
        }
    });
}
