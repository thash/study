fn main() {
    let an_integer = 1u; // u = unsigned int
    // let a_boolean = true;
    // let unit = (); // is it same as unit in OCaml?

    let copied_integer = an_integer;

    // default immutable.
    let mut mutable_integer = 1u;

    println!("before mutation: {}", mutable_integer);
    mutable_integer += 1;
    println!("after mutation: {}", mutable_integer);


    // next: http://rustbyexample.com/variables/scope.html
}
