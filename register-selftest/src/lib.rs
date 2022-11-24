#![no_std]
use core::ptr::*;

pub struct TestCase<'a> {
    pub function: fn(),
    pub addr: usize,
    pub uid: &'a str,
}

include!(concat!(env!("OUT_DIR"), "/register_selftest.rs"));
