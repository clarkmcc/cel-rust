use common_expression_language::Program;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use common_expression_language::context::Context;

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    // if rl.load_history("history.txt").is_err() {
    //     println!("No previous history.");
    // }
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let program = Program::compile(&line).unwrap();
                let ctx = Context::default();
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
    rl.save_history("history.txt").unwrap();
}
//
// fn main() {
//     // println!("{:?}", ExpressionParser::new().parse("((TestDouble >= 1.0 || TestString.TestFunction() == 'HelloWorld') && (TestDouble + 1.0 >= 0.0)) || Now > TestTime").unwrap_or_else(|e| panic!("{}", e)));
//     println!(
//         "{:?}",
//         ExpressionParser::new()
//             .parse("a.b[1]")
//             .unwrap_or_else(|e| panic!("{}", e))
//     );
//     // println!("{:?}", ExpressionParser::new().parse("\"abclol\"").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse(r"has(account.properties.id)").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse(r"has(account.properties.id) && (type(account.properties.id) == string || type(account.properties.id) == list(string))").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse(r#"'abc'"#).unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("\"\"\"abc\nlol\"\"\"").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse(r#"'''abc'''"#).unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("-1E3").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("a.b(c + 3 / 2,) == 2").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("[1,2,3, abc, ]").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("Account{user_id: 123}").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("{1 + a: 3}").unwrap_or_else(|e| panic!("{}", e)));
//     // println!("{:?}", ExpressionParser::new().parse("Account{user_id: 123}.user_id == 123").unwrap_or_else(|e| panic!("{}", e)));
//     println!("{}", size_of::<Expression>());
//     println!("{}", size_of::<Member>());
//     println!("{}", size_of::<Atom>());
// }
