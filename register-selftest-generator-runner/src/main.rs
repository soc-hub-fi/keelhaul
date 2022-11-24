use register_selftest;

fn main() {
    // TODO: enum TestType { Read, Reset, }

    for (index, function) in register_selftest::FUNCTIONS.iter().enumerate() {
        let result = function();
        println!("{index}: {result}");
    }
}
