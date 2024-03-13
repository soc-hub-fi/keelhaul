#![no_std]
#![no_main]

use hello_hpc::uart_write;
use panic_halt as _;
use riscv_rt::entry;

#[entry]
fn main() -> ! {
    uart_write("Hello world!");
    loop {}
}
