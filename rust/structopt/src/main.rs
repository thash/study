use structopt::StructOpt;

#[derive(StructOpt, Debug)]
enum Nanika {
    List,
    Scan,
}

fn main() {
    println!("Hello, world!");
}