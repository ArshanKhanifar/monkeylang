use repl::start;
use std::io;

fn main() {
    let username = whoami::username();
    println!(
        "Hello, {}! This is the monkey programming language!",
        username
    );
    println!("Feel free to type in commands.");
    start(io::stdin(), io::stdout());
}
