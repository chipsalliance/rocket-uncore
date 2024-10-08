#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// This disable warning of `u128` type usage.
// The `u128` type is not ABI stable, and is considered not FFI safe.
#![allow(improper_ctypes)]

include!(concat!(env!("OUT_DIR"), "/sail_h.rs"));
