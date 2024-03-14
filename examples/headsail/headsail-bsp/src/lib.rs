//! A light-weight memory map based board support package for Headsail.
#![no_std]

pub mod sprintln;
pub mod uart;

#[cfg(any(feature = "rt"))]
pub use riscv_rt as rt;
pub use ufmt;

mod mmap;
mod ufmt_panic;

/// # Safety
///
/// Unaligned reads may fail to produce expected results on RISC-V.
#[inline(always)]
pub unsafe fn read_u8(addr: usize) -> u8 {
    core::ptr::read_volatile(addr as *const _)
}

/// # Safety
///
/// Unaligned writes may fail to produce expected results on RISC-V.
#[inline(always)]
pub unsafe fn write_u8(addr: usize, val: u8) {
    core::ptr::write_volatile(addr as *mut _, val)
}

#[inline(always)]
pub fn read_u32(addr: usize) -> u32 {
    unsafe { core::ptr::read_volatile(addr as *const _) }
}

#[inline(always)]
pub fn write_u32(addr: usize, val: u32) {
    unsafe { core::ptr::write_volatile(addr as *mut _, val) }
}

#[cfg(feature = "sysctrl-rt")]
#[export_name = "_setup_interrupts"]
fn setup_interrupt_vector() {
    use riscv::register::mtvec;

    // Set the trap vector
    unsafe {
        extern "C" {
            fn _trap_vector();
        }

        // Set all the trap vectors for good measure
        let bits = _trap_vector as usize;
        mtvec::write(bits, mtvec::TrapMode::Vectored);
    }
}

// The vector table
//
// Do the ESP trick and route all interrupts to the direct dispatcher.
//
// N.b. vectors length must be exactly 0x80
#[cfg(feature = "sysctrl-rt")]
core::arch::global_asm!(
    "
.section .vectors, \"ax\"
    .global _trap_vector
    // Trap vector base address must always be aligned on a 4-byte boundary
    .align 4
_trap_vector:
    j _start_trap
    .rept 31
    .word _start_trap // 1..31
    .endr
"
);
