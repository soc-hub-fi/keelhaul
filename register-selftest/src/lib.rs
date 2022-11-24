#![no_std]
use core::ptr::*;

pub struct TestCase<'a> {
    pub function8: Option<fn() -> u8>,
    pub function16: Option<fn() -> u16>,
    pub function32: Option<fn() -> u32>,
    pub function64: Option<fn() -> u64>,
    pub addr: usize,
    pub uid: &'a str,
}

include!(concat!(env!("OUT_DIR"), "/register_selftest.rs"));
