// Some utilities that fasciliate Crust-style programming: https://github.com/tsoding/Crust
use crate::libc;
use core::slice;
use core::ptr;

#[macro_export]
macro_rules! non_soy_enum {
    (iota = $value:expr,) => {};
    (iota = $value:expr, $name:ident, $($tail:tt)*) => {
        pub const $name: c_long = $value;
        non_soy_enum!(iota = $value + 1, $($tail)*);
    };
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Array<T> {
    pub items: *mut T,
    pub count: usize,
    pub capacity: usize,
}

pub unsafe fn array_slice<T>(xs: Array<T>) -> *mut [T] {
    slice::from_raw_parts_mut(xs.items, xs.count)
}

pub unsafe fn array_push<T>(xs: *mut Array<T>, item: T) {
    if (*xs).count >= (*xs).capacity {
        if (*xs).capacity == 0 {
            (*xs).capacity = 256;
        } else {
            (*xs).capacity *= 2;
        }
        (*xs).items = libc::realloc_items((*xs).items, (*xs).capacity);
    }
    *((*xs).items.add((*xs).count)) = item;
    (*xs).count += 1;
}

pub unsafe fn array_destroy<T>(xs: *mut Array<T>) {
    libc::free((*xs).items);
    (*xs).items = ptr::null_mut();
    (*xs).count = 0;
    (*xs).capacity = 0;
}
