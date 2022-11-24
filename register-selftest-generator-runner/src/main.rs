use register_selftest;

fn main() {
    // TODO: enum TestType { Read, Reset, }

    for (index, test_case) in register_selftest::HPC::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
    }

    /*for (index, test_case) in register_selftest::TEST_CASES.iter().enumerate() {
        //let name = test_case.name;
        let result = (test_case.function)();
        //println!("{index} {name}: {result}");
    }*/
}
