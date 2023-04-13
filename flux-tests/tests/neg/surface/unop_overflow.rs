#![feature(register_tool, custom_inner_attributes)]
#![register_tool(flux)]
#![flux::cfg(check_overflow = true)]

#[flux::sig(fn(a: i32) -> i32[-a])]
pub fn neg_overflow_i32(a: i32) -> i32 {
    -a //~ ERROR overflow
}
