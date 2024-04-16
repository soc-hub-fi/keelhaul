use register_selftest::Error;

fn main() {
    for test_case in register_selftest::TEST_CASES.iter() {
        println!("{}", test_case.uid);
        (test_case.function)().unwrap_or_else(|err| match err {
            Error::ReadValueIsNotResetValue {
                read_val,
                reset_val,
                reg_uid,
                reg_addr,
            } => {
                println!("read value '{read_val}' was not expected reset val '{reset_val}' in register '{reg_uid}'@{reg_addr}");
            }
        });
    }
}
