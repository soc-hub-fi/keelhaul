use register_selftest;

fn main() {
    // TODO: enum TestType { Read, Reset, }

    for (_index, test_case) in register_selftest::hpc::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
    }

    for (_index, test_case) in register_selftest::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
        if let Some(function) = test_case.function32 {
            let result = (function)();
        }
    }
}
