fn main() {
    // TODO: enum TestType { Read, Reset, }

    /*
    for (_index, test_case) in register_selftest::hpc::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
    }
    */

    for (_index, test_case) in register_selftest::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
        (test_case.function)().unwrap_or_else(|err| match err {
            _ => todo!(),
        });
    }
}
