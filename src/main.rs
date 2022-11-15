/*const SVD: &str = include_str!("asd.svd");

mod reg_self_test {
    fn test(test_types, exludes) {
        let xml = SVD.parse();
        for test_type in test_types {
            for p in xml-peripherals() {
                fr c in p.clusters() {
                    for reg in c.registers() {
                        println!/("addr {}, reset_value {}", reg.addre(), reg.reset_value());
                    }
                }
            }
        }
    }

    fn test_asd() {
        for reg in regs {
            use core::ptr;
            ptr::write_volatile()reg;
        }
    }

    enum TestType {
        Read,
        Reset,
    }
}*/

// NEEDS: build.rs with python script that generates

mod register_tests;

//use register_tests;

fn main() {
    for (index, function) in register_tests::FUNCTIONS.iter().enumerate() {
        let result = function();
        println!("{index}: {result}");
    }

    /*let excludes = [""];

    // ONLY reg_self_test::test can be generated!
    reg_self_test::test([TestType::Read, TestType::Reset], excludes);*/
}
