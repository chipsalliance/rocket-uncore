#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
// This disable warning of `u128` type usage.
// The `u128` type is not ABI stable, and is considered not FFI safe.
#![allow(improper_ctypes)]

// #include <sail.h>
include!(concat!(env!("OUT_DIR"), "/sail_h.rs"));

#[repr(C)]
pub(crate) struct zMisa {
  zMisa_chunk_0: mach_bits,
}

#[repr(C)]
pub(crate) struct zMcause {
  zMcause_chunk_0: mach_bits,
}

// ----------------------------------------------------------------------------
// Defined in sail model
// ----------------------------------------------------------------------------
extern "C" {
  fn model_init();
  fn model_fini();

  fn z_set_Misa_C(_: *mut zMisa, _: mach_bits) -> unit;
  fn z_set_Misa_D(_: *mut zMisa, _: mach_bits) -> unit;
  fn z_set_Misa_F(_: *mut zMisa, _: mach_bits) -> unit;
}

// ----------------------------------------------------------------------------
// Defined in sail-riscv model
// ----------------------------------------------------------------------------
extern "C" {
  fn zinit_model(_: unit) -> unit;
  fn zstep(_: sail_int) -> bool;
  fn ztick_clock(_: unit) -> unit;
  fn ztick_platform(_: unit) -> unit;

  pub(crate) static mut zxlen_val: mach_bits;
  pub(crate) static mut zhtif_done: bool;
  pub(crate) static mut zhtif_exit_code: mach_bits;
  pub(crate) static mut have_exception: bool;

  /* machine state */
  pub(crate) static mut zcur_privilege: u32;
  pub(crate) static mut zPC: mach_bits;
  pub(crate) static mut zx1: mach_bits;
  pub(crate) static mut zx2: mach_bits;
  pub(crate) static mut zx3: mach_bits;
  pub(crate) static mut zx4: mach_bits;
  pub(crate) static mut zx5: mach_bits;
  pub(crate) static mut zx6: mach_bits;
  pub(crate) static mut zx7: mach_bits;
  pub(crate) static mut zx8: mach_bits;
  pub(crate) static mut zx9: mach_bits;
  pub(crate) static mut zx10: mach_bits;
  pub(crate) static mut zx11: mach_bits;
  pub(crate) static mut zx12: mach_bits;
  pub(crate) static mut zx13: mach_bits;
  pub(crate) static mut zx14: mach_bits;
  pub(crate) static mut zx15: mach_bits;
  pub(crate) static mut zx16: mach_bits;
  pub(crate) static mut zx17: mach_bits;
  pub(crate) static mut zx18: mach_bits;
  pub(crate) static mut zx19: mach_bits;
  pub(crate) static mut zx20: mach_bits;
  pub(crate) static mut zx21: mach_bits;
  pub(crate) static mut zx22: mach_bits;
  pub(crate) static mut zx23: mach_bits;
  pub(crate) static mut zx24: mach_bits;
  pub(crate) static mut zx25: mach_bits;
  pub(crate) static mut zx26: mach_bits;
  pub(crate) static mut zx27: mach_bits;
  pub(crate) static mut zx28: mach_bits;
  pub(crate) static mut zx29: mach_bits;
  pub(crate) static mut zx30: mach_bits;
  pub(crate) static mut zx31: mach_bits;

  pub(crate) static mut zmstatus: mach_bits;
  pub(crate) static mut zmepc: mach_bits;
  pub(crate) static mut zmtval: mach_bits;
  pub(crate) static mut zsepc: mach_bits;
  pub(crate) static mut zstval: mach_bits;

  pub(crate) static mut zfloat_result: mach_bits;
  pub(crate) static mut zfloat_fflags: mach_bits;

  pub(crate) static mut zmcause: zMcause;
  pub(crate) static mut zscause: zMcause;

  pub(crate) static mut zminstret: mach_bits;

  pub(crate) static mut zmisa: zMisa;
}
