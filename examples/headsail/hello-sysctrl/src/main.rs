#![no_std]
#![no_main]

use headsail_bsp::{rt::entry, uart::uart_write};
use panic_halt as _;

#[entry]
fn main() -> ! {
    uart_write("Hello world!");
    loop {}
}
