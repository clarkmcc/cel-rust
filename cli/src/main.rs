use anyhow::Result;
use cel_interpreter::context::Context;
use cel_interpreter::objects::CelType;
use cel_interpreter::Program;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() -> Result<()> {
    let mut rl = Editor::<()>::new();
    let repl_history_path = match dirs::config_dir() {
        Some(mut path) => {
            path.push("cel-cli");
            path.push("repl_history.txt");
            path
        }
        None => ".repl_history.txt".into(),
    };
    if let Some(parent) = repl_history_path.parent() {
        if !parent.is_dir() {
            std::fs::create_dir_all(parent)?;
        }
    }
    if rl.load_history(&repl_history_path).is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let program = Program::compile(&line).unwrap();
                let mut ctx = Context::default();
                ctx.variables
                    .insert("TestDouble".into(), CelType::Float(0.0f64));
                ctx.variables.insert(
                    "TestString".into(),
                    CelType::String("World".to_string().into()),
                );
                ctx.variables.insert("TestTime".into(), CelType::UInt(0));
                ctx.variables.insert("Now".into(), CelType::UInt(1));
                ctx.add_function("TestFunction".into(), |target, args, ctx| match target {
                    Some(CelType::String(v)) => CelType::String(format!("Hello{}", v).into()),
                    _ => unreachable!(),
                });
                println!("{:?}", program);
                println!("{:?}", program.execute(&ctx));
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history(&repl_history_path).unwrap();
    Ok(())
}
