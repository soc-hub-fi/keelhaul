//mod old_register_tests;

use register_selftest_generator;

fn main() {
    // TODO: enum TestType { Read, Reset, }
    // TODO: excludes

    //for (index, function) in old_register_tests::FUNCTIONS.iter().enumerate() {
    for (index, function) in register_selftest_generator::FUNCTIONS.iter().enumerate() {
        let result = function();
        println!("{index}: {result}");
    }
}
