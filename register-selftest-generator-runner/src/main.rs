use register_selftest::{self, FuncRet};

fn main() {
    // TODO: enum TestType { Read, Reset, }

    for (_index, test_case) in register_selftest::hpc::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
    }

    for (_index, test_case) in register_selftest::TEST_CASES.iter().enumerate() {
        println!("{}", test_case.uid);
        let result = (test_case.function)();
        match result {
            FuncRet::U8(byte) => {}
            _ => {}
        }
    }
}
