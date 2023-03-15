#![no_std]
use core::ptr::*;

/// Represents a test case for a single register.
pub struct TestCase<'a> {
    pub function: fn(),
    pub addr: usize,
    pub uid: &'a str,
}

include!(concat!(env!("OUT_DIR"), "/register_selftest.rs"));
