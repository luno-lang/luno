use clap::Parser;

#[derive(Parser)]
#[command(version = "1.0", author = "Luno")]
struct Args {
    #[arg(short = 'c', long = "compile")]
    compile_file: Option<String>,
}

fn main() {
    println!("Luno v0.0.1");
}
