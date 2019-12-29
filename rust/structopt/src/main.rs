use structopt::StructOpt;

#[derive(StructOpt, Debug)]
enum Nanika {
    List {
        // we don't want to name it "speed", need to look smart
        // $ cargo run list -v 32 // => List { speed: 32.0 }
        #[structopt(short = "v", long = "velocity", default_value = "42")]
        speed: f64,
    },
    Zod {
        // default value is set to false for bool options.
        #[structopt(long = "myflag")]
        myflag: bool
    }
}

fn main() {
    let nanika = Nanika::from_args();
    println!("{:?}", nanika);
    // println!("{:?}", nanika.speed);
    match nanika {
        Nanika::List { speed } => println!("speed is...! {:?}", speed),
        Nanika::Zod  { myflag } => println!("flag is {:?}", myflag),
    }
}
