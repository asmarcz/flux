pub trait Trait {
    type A;
}

impl Trait for i32 {
    type A = i32;
}

pub fn foo(x: &i32) -> <i32 as Trait>::A {
    *x
}
