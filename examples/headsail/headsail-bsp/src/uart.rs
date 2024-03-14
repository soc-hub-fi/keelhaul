use crate::{mmap::UART0_ADDR, write_u8};

#[inline]
pub fn uart_write(s: &str) {
    for b in s.as_bytes() {
        putc(*b);
    }
}

#[inline]
fn putc(c: u8) {
    // Safety: we don't know if u8 writes work for all target architectures
    unsafe { write_u8(UART0_ADDR, c as u8) };
}
