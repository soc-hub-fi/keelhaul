//! Register test case generator.
//!
//! # How to use
//!
//! Provide path to parser result and output via environment variables.
//!
//! `PATH_INPUT=temp/parsed.json PATH_OUTPUT=runner/src/register_tests.rs cargo run`

use common::Register;
use json::JsonValue;
use std::{
    env,
    fs::{self, read_to_string},
    io::Write,
};

/// Extract registers from JSON object.
fn get_registers(content: JsonValue) -> Vec<Register> {
    let mut registers = Vec::new();
    match content {
        JsonValue::Array(array) => {
            for value in array {
                let register = match value {
                    JsonValue::Object(object) => Register {
                        name: object
                            .get("name")
                            .expect("JSON object does not contain 'name'-field.")
                            .to_string(),
                        address_base: object
                            .get("address_base")
                            .expect("JSON object does not contain 'address_base'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'address_base'-field as integer."),
                        address_offset_cluster: object
                            .get("address_offset_cluster")
                            .expect("JSON object does not contain 'address_offset_cluster'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'address_offset_cluster'-field as integer."),
                        address_offset_register: object
                            .get("address_offset_register")
                            .expect("JSON object does not contain 'address_offset_register'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'address_offset_register'-field as integer."),
                        value_reset: object
                            .get("value_reset")
                            .expect("JSON object does not contain 'value_reset'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'value_reset'-field as integer."),
                        can_read: object
                            .get("can_read")
                            .expect("JSON object does not contain 'can_read'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'can_read'-field as integer."),
                        can_write: object
                            .get("can_write")
                            .expect("JSON object does not contain 'can_write'-field.")
                            .to_string()
                            .parse()
                            .expect("Failed to parse 'can_write'-field as integer."),
                    },
                    _ => panic!("Illegal JSON object type."),
                };
                registers.push(register);
            }
        }
        _ => panic!("Illegal JSON object type."),
    }
    registers
}

fn main() {
    let path_input = env::var("PATH_INPUT").expect("Missing environment variable: PATH_INPUT");
    let path_output = env::var("PATH_OUTPUT").expect("Missing environment variable: PATH_OUTPUT");
    let mut file_output = fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(path_output)
        .expect("Failed to open output file.");

    let content = read_to_string(path_input).expect("Failed to read parser results.");
    let content = json::parse(&content).expect("Failed to parse parser results.");

    let registers = get_registers(content);

    let mut output = Vec::new();
    let mut function_names = Vec::new();
    for register in registers {
        let function_name = format!("test_{}_{}", register.name, register.full_address());
        let mut statements = vec![format!(
            "#[allow(unused)] let address: *mut u32 = {} as *mut u32;",
            register.full_address()
        )];
        if register.can_read {
            statements.push("let _ = unsafe { read_volatile(address) };".to_owned());
        }
        if register.can_write {
            statements.push(format!("let reset_value = {};", register.value_reset));
            statements.push("unsafe { write_volatile(address, reset_value) };".to_owned());
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
    let output = output.join("");
    let function_count = function_names.len();
    let function_names = function_names.join(",");
    let function_array = format!(
        "pub static FUNCTIONS: [fn()->u32;{}] = [{}];",
        function_count, function_names
    );
    let lines = vec![
        "use core::ptr::read_volatile;\n",
        "use core::ptr::write_volatile;\n",
        &output,
        &function_array,
    ];
    for line in lines {
        file_output
            .write_all(line.as_bytes())
            .expect("Failed to write to output file.");
    }
    println!("Wrote {} test cases.", function_count);
}
