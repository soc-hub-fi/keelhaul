#![no_std]
#![no_main]

use core::{mem, ptr};
use headsail_bsp::{rt::entry, sprintln};

// TODO: add exception handler

mod constants {
    pub(crate) const CHECK_FOR_POINTER_ALIGNMENT: bool = false;

    pub(crate) const MINIMIZE_OUTPUT_ENV: &str = match option_env!("MIN_OUTPUT") {
        Some(v) => v,
        None => "0",
    };
}
use constants::*;

// Custom memory map based on Excel
mod memorymap {
    #![allow(dead_code)]
    pub(crate) const SYSCTRL_BASE: u32 = 0x1a10_4000;
    pub(crate) const SS_CLK_CTRL1: *mut u32 = (SYSCTRL_BASE + 0x98) as *mut u32;
    pub(crate) const SS_CLK_CTRL2: *mut u32 = (SYSCTRL_BASE + 0x9c) as *mut u32;
    pub(crate) const SS_CLK_CTRL3: *mut u32 = (SYSCTRL_BASE + 0xb8) as *mut u32;
    pub(crate) const SS_RESET_EN: *mut u32 = (SYSCTRL_BASE + 0xb0) as *mut u32;

    pub(crate) const ETH_CLK_CTRL1_FIELD_OFFSET: u32 = 0;
    pub(crate) const DLA_CLK_CTRL1_FIELD_OFFSET: u32 = 8;
    pub(crate) const HPC_CLK_CTRL1_FIELD_OFFSET: u32 = 16;
    pub(crate) const DDR2_CLK_CTRL1_FIELD_OFFSET: u32 = 24;

    pub(crate) const DSP_CLK_CTRL2_FIELD_OFFSET: u32 = 0;
    pub(crate) const ICN_CLK_CTRL2_FIELD_OFFSET: u32 = 8;
    pub(crate) const C2C_SER_CLK_CTRL2_FIELD_OFFSET: u32 = 16;
    pub(crate) const C2C_PAR_CLK_CTRL2_FIELD_OFFSET: u32 = 24;

    pub(crate) const PERIPH_CLK_CTRL3_FIELD_OFFSET: u32 = 0;

    // Structure bits for each subsystem in CLK_CTRL* registers
    pub(crate) const CLK_CTRL_SEL_CKA_BIT: u32 = 1 << 0;
    pub(crate) const CLK_CTRL_FORCE_CKA_BIT: u32 = 1 << 1;
    pub(crate) const CLK_CTRL_FORCE_CKB_BIT: u32 = 1 << 2;
    pub(crate) const CLK_CTRL_CLKENA_BIT: u32 = 1 << 3;
    pub(crate) const CLK_CTRL_HPC_CORE0_BIT: u32 = 1 << 4;
    pub(crate) const CLK_CTRL_HPC_CORE1_BIT: u32 = 1 << 5;
    pub(crate) const CLK_CTRL_PLL_CTRL_VALID_BIT: u32 = 1 << 7;
}
use memorymap::*;

fn align_error(uid: &str, addr: usize) {
    sprintln!(
        "[ERROR] {} at 0x{:x} is not {}-byte aligned",
        uid,
        addr,
        usize::BITS / 8,
    );
}

fn modify(reg: *mut u32, mask: u32) {
    let mut val = unsafe { ptr::read_volatile(reg) };
    val |= mask;
    unsafe { ptr::write_volatile(reg, val) };
}

#[entry]
fn main() -> ! {
    sprintln!("[{}]", env!("CARGO_PKG_NAME"));

    sprintln!("Activate everything...");
    modify(SS_RESET_EN, 0xffff_ffff);
    modify(
        SS_CLK_CTRL1,
        (CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << DDR2_CLK_CTRL1_FIELD_OFFSET
            | ((CLK_CTRL_SEL_CKA_BIT
                | CLK_CTRL_CLKENA_BIT
                | CLK_CTRL_HPC_CORE0_BIT
                | CLK_CTRL_HPC_CORE1_BIT)
                << HPC_CLK_CTRL1_FIELD_OFFSET)
            | ((CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << DLA_CLK_CTRL1_FIELD_OFFSET)
            | ((CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << ETH_CLK_CTRL1_FIELD_OFFSET),
    );
    modify(
        SS_CLK_CTRL2,
        ((CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << DSP_CLK_CTRL2_FIELD_OFFSET)
            | ((CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << ICN_CLK_CTRL2_FIELD_OFFSET)
            | ((CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << C2C_SER_CLK_CTRL2_FIELD_OFFSET)
            | ((CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << C2C_PAR_CLK_CTRL2_FIELD_OFFSET),
    );
    modify(
        SS_CLK_CTRL3,
        (CLK_CTRL_SEL_CKA_BIT | CLK_CTRL_CLKENA_BIT) << PERIPH_CLK_CTRL3_FIELD_OFFSET,
    );
    sprintln!("Done");

    let cases = &register_selftest::TEST_CASES;
    let test_count = cases.len();
    sprintln!("Test count: {}", test_count);

    // Minimize output for CI
    let min_output: bool = MINIMIZE_OUTPUT_ENV != "0";

    for (case_idx, case) in cases.iter().enumerate() {
        if min_output {
            sprintln!("{}", case_idx + 1);
        } else {
            sprintln!(
                "0x{:x} := {} ({}/{})...",
                case.addr,
                case.uid,
                case_idx + 1,
                test_count
            );
        }
        if CHECK_FOR_POINTER_ALIGNMENT
            && (case.addr as *const usize).align_offset(mem::align_of::<usize>()) != 0
        {
            align_error(case.uid, case.addr);
            continue;
        }
        if (case.function)().is_err() {
            panic!();
        }
        sprintln!("> OK");
    }

    sprintln!("[ok]");

    loop {
        unsafe { core::arch::asm!("wfi") };
    }
}
