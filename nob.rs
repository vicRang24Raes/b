use core::ffi::*;

#[macro_export]
macro_rules! shift {
    ($ptr:ident, $len:ident) => {{
        let result = *$ptr;
        $ptr = $ptr.add(1);
        $len -= 1;
        result
    }};
}

#[repr(C)]
pub struct String_Builder {
    pub items: *mut c_char,
    pub count: usize,
    pub capacity: usize,
}

extern "C" {
    #[link_name = "nob_read_entire_file"]
    pub fn read_entire_file(path: *const c_char, sb: *mut String_Builder) -> bool;
    #[link_name = "nob_write_entire_file"]
    pub fn write_entire_file(path: *const c_char, data: *const c_void, size: usize) -> bool;
    #[link_name = "nob_temp_sprintf"]
    pub fn temp_sprintf(format: *const c_char, ...) -> *mut c_char;
    #[link_name = "nob_sb_appendf"]
    pub fn sb_appendf(sb: *mut String_Builder, fmt: *const c_char, ...) -> c_int;
}
