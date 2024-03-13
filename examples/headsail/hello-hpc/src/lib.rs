#![no_std]

mod mmap;

use core::ptr;
use mmap::UART0_ADDR;

pub fn uart_write(s: &str) {
    for b in s.as_bytes() {
        unsafe { ptr::write_volatile(UART0_ADDR as *mut u8, *b) };
    }
}
