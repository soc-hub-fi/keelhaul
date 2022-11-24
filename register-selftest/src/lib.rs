#![no_std]
use core::ptr::*;

#[non_exhaustive]
pub enum FuncRet {
    None,
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

pub struct TestCase<'a> {
    pub function: fn() -> FuncRet,
    pub addr: usize,
    pub uid: &'a str,
}

include!(concat!(env!("OUT_DIR"), "/register_selftest.rs"));
