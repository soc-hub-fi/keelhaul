#![no_std]
use core::ptr::*;

pub struct TestCase<'a> {
    pub function: fn() -> u32,
    pub addr: usize,
    pub uid: &'a str,
}

include!(concat!(env!("OUT_DIR"), "/register_selftest.rs"));
