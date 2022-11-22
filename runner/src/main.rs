mod register_tests;

fn main() {
    // TODO: enum TestType { Read, Reset, }
    // TODO: excludes

    for (index, function) in register_tests::FUNCTIONS.iter().enumerate() {
        let result = function();
        println!("{index}: {result}");
    }
}
