use common::Register;
use json::JsonValue;
use std::env;
use std::fs;
use std::fs::read_to_string;
use std::io::Write;

fn main() {
    let path_input = env::var("PATH_INPUT").unwrap();
    let path_output = env::var("PATH_OUTPUT").unwrap();
    let mut file_output = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path_output)
        .unwrap();

    let content = read_to_string(path_input).unwrap();
    let content = json::parse(&content).unwrap();

    let mut registers = Vec::new();
    match content {
        JsonValue::Array(array) => {
            for value in array {
                let register = match value {
                    JsonValue::Object(o) => Register {
                        name: o.get("name").unwrap().to_string(),
                        address_base: o.get("address_base").unwrap().to_string().parse().unwrap(),
                        address_offset_cluster: o
                            .get("address_offset_cluster")
                            .unwrap()
                            .to_string()
                            .parse()
                            .unwrap(),
                        address_offset_register: o
                            .get("address_offset_register")
                            .unwrap()
                            .to_string()
                            .parse()
                            .unwrap(),
                        value_reset: o.get("value_reset").unwrap().to_string().parse().unwrap(),
                        can_read: o.get("can_read").unwrap().to_string().parse().unwrap(),
                        can_write: o.get("can_write").unwrap().to_string().parse().unwrap(),
                    },
                    _ => panic!(),
                };
                registers.push(register);
            }
        }
        _ => panic!(),
    }

    let mut output = Vec::new();
    let mut function_names = Vec::new();
    for register in registers {
        let address = register.address_base
            + register.address_offset_cluster
            + register.address_offset_register;
        let function_name = format!("test_{}_{}", register.name, address);
        let mut statements = vec![format!(
            "#[allow(unused)] let address: *mut u32 = {} as *mut u32;",
            address
        )];
        if register.can_read {
            statements.push("let _ = unsafe { read_volatile(address) };".to_string());
        }
        if register.can_write {
            statements.push(format!("let reset_value = {};", register.value_reset));
            statements.push("unsafe { write_volatile(address, reset_value) };".to_string());
        }
        let statements = statements.join("");
        let statements = format!("{} 0", statements);
        let line = format!(
            "#[allow(non_snake_case)] pub fn {}() -> u32 {{{}}}\n",
            function_name, statements
        );
        output.push(line);
        function_names.push(function_name);
    }
    let function_count = function_names.len();
    let function_names = function_names.join(",");
    file_output
        .write_all("use core::ptr::read_volatile;\n".as_bytes())
        .unwrap();
    file_output
        .write_all("use core::ptr::write_volatile;\n".as_bytes())
        .unwrap();
    file_output.write_all(output.join("").as_bytes()).unwrap();
    file_output
        .write_all(
            format!(
                "pub static FUNCTIONS: [fn()->u32;{}] = [{}];",
                function_count, function_names
            )
            .as_bytes(),
        )
        .unwrap();
    println!("Wrote {} test cases.", function_count);
}
