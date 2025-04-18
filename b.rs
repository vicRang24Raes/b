#![no_main]
#![no_std]
extern crate core;

use core::panic::PanicInfo;
use core::ffi::{c_char};
use core::mem::zeroed;
use core::ptr;

#[panic_handler]
unsafe fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[macro_use]
pub mod libc {
    use core::ffi::*;

    pub type FILE = c_void;

    extern "C" {
        pub static stdin: *mut FILE;
        pub static stdout: *mut FILE;
        pub static stderr: *mut FILE;
        pub fn strcmp(s1: *const c_char, s2: *const c_char) -> c_int;
        pub fn abort() -> !;
        pub fn strdup(s: *const c_char) -> *mut c_char;
    }

    #[macro_export]
    macro_rules! printf {
        ($fmt:literal $($args:tt)*) => {{
            use core::ffi::c_int;
            extern "C" {
                #[link_name = "printf"]
                pub fn printf_raw(fmt: *const c_char, ...) -> c_int;
            }
            printf_raw($fmt.as_ptr() $($args)*)
        }};
    }

    #[macro_export]
    macro_rules! fprintf {
        ($stream:expr, $fmt:literal $($args:tt)*) => {{
            use core::ffi::c_int;
            extern "C" {
                #[link_name = "fprintf"]
                pub fn fprintf_raw(stream: *mut libc::FILE, fmt: *const c_char, ...) -> c_int;
            }
            fprintf_raw($stream, $fmt.as_ptr() $($args)*)
        }};
    }

    pub unsafe fn realloc<T>(ptr: *mut T, count: usize) -> *mut T {
        extern "C" {
            #[link_name = "realloc"]
            fn realloc_raw(ptr: *mut c_void, size: usize) -> *mut c_void;
        }
        realloc_raw(ptr as *mut c_void, size_of::<T>()*count) as *mut T
    }

    pub unsafe fn free<T>(ptr: *mut T) {
        extern "C" {
            #[link_name = "free"]
            fn free_raw(ptr: *mut c_void);
        }
        free_raw(ptr as *mut c_void);
    }
}

#[macro_use]
pub mod nob {
    use core::ffi::{c_char};

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
    }
}

pub mod stb_c_lexer {
    use core::ffi::{c_char, c_long, c_double, c_int};

    #[repr(C)]
    #[allow(non_camel_case_types)]
    pub enum CLEX {
        eof = 256,
        parse_error,
        intlit        ,
        floatlit      ,
        id            ,
        dqstring      ,
        sqstring      ,
        charlit       ,
        eq            ,
        noteq         ,
        lesseq        ,
        greatereq     ,
        andand        ,
        oror          ,
        shl           ,
        shr           ,
        plusplus      ,
        minusminus    ,
        pluseq        ,
        minuseq       ,
        muleq         ,
        diveq         ,
        modeq         ,
        andeq         ,
        oreq          ,
        xoreq         ,
        arrow         ,
        eqarrow       ,
        shleq,
        shreq,
        first_unused_token
    }

    #[repr(C)]
    pub struct stb_lexer {
        // lexer variables
        pub input_stream   : *mut c_char,
        pub eof            : *mut c_char,
        pub parse_point    : *mut c_char,
        pub string_storage : *mut c_char,
        pub string_storage_len: c_int,

        // lexer parse location for error messages
        pub where_firstchar: *mut c_char,
        pub where_lastchar: *mut c_char,

        // lexer token variables
        pub token: c_long,
        pub real_number: c_double,
        pub int_number: c_long,
        pub string: *mut c_char,
        pub string_len: c_int,
    }

    #[repr(C)]
    pub struct stb_lex_location {
        pub line_number: c_int,
        pub line_offset: c_int,
    }

    extern "C" {
        #[link_name="stb_c_lexer_init"]
        pub fn init(lexer: *mut stb_lexer, input_stream: *const c_char, input_stream_end: *const c_char, string_store: *mut c_char, store_length: c_int);
        #[link_name="stb_c_lexer_get_token"]
        pub fn get_token(lexer: *mut stb_lexer) -> c_int;
        #[link_name="stb_c_lexer_get_location"]
        pub fn get_location(lexer: *const stb_lexer, hwere: *const c_char, loc: *mut stb_lex_location);
    }
}

pub mod ds { // Data Structures
    use crate::libc;
    use core::slice;
    use core::ptr;

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
            (*xs).items = libc::realloc((*xs).items, (*xs).capacity);
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
}

unsafe fn expect_clex(l: *const stb_c_lexer::stb_lexer, input_path: *const c_char, clex: i64) -> bool {
    if (*l).token != clex {
        let mut loc: stb_c_lexer::stb_lex_location = zeroed();
        stb_c_lexer::get_location(l, (*l).where_firstchar, &mut loc);
        // TODO: print tokens as human readable
        fprintf!(libc::stderr, c"%s:%d:%d: ERROR: expected %ld, but got %ld\n", input_path, loc.line_number, loc.line_offset + 1, clex, (*l).token);
        return false
    }
    true
}

unsafe fn get_and_expect_clex(l: *mut stb_c_lexer::stb_lexer, input_path: *const c_char, clex: i64) -> bool {
    stb_c_lexer::get_token(l);
    expect_clex(l, input_path, clex)
}

#[derive(Clone, Copy)]
struct AutoVar {
    name: *const c_char,
    offset: usize,
    hwere: *mut c_char,
}

unsafe fn find_auto_var(vars: *mut [AutoVar], name: *const c_char) -> *mut AutoVar {
    for i in 0..vars.len() {
        if libc::strcmp((*vars)[i].name, name) == 0 {
            return &mut (*vars)[i];
        }
    }
    return ptr::null_mut();
}

unsafe fn usage(program_name: *const c_char) {
    fprintf!(libc::stderr, c"Usage: %s <input.b>\n", program_name);
}

#[no_mangle]
unsafe extern "C" fn main(mut _argc: i32, mut _argv: *mut *mut c_char) -> i32 {
    let program_name = shift!(_argv, _argc);

    if _argc <= 0 {
        usage(program_name);
        fprintf!(libc::stderr, c"ERROR: no input is provided\n");
        return 69;
    }

    let input_path = shift!(_argv, _argc);

    let mut vars: ds::Array<AutoVar> = zeroed();
    let mut vars_offset: usize;

    let mut sb: nob::String_Builder = zeroed();
    if !nob::read_entire_file(input_path, &mut sb) { return 1; }
    let mut l: stb_c_lexer::stb_lexer = zeroed();
    let mut string_store: [c_char; 1024] = zeroed();
    stb_c_lexer::init(&mut l, sb.items, sb.items.add(sb.count), string_store.as_mut_ptr(), string_store.len() as i32);

    printf!(c"format ELF64\n");
    printf!(c"section \".text\" executable\n");

    'func: loop {
        vars.count = 0;
        vars_offset = 0;

        stb_c_lexer::get_token(&mut l);
        if l.token == stb_c_lexer::CLEX::eof as i64 { break 'func }

        if !expect_clex(&mut l, input_path, stb_c_lexer::CLEX::id as i64) { return 1; }
        printf!(c"public %s\n", l.string);
        printf!(c"%s:\n", l.string);
        if !get_and_expect_clex(&mut l, input_path, '(' as i64) { return 1; }
        if !get_and_expect_clex(&mut l, input_path, ')' as i64) { return 1; }
        if !get_and_expect_clex(&mut l, input_path, '{' as i64) { return 1; }

        printf!(c"    push rbp\n");
        printf!(c"    mov rbp, rsp\n");

        'body: loop {
            // Statement
            stb_c_lexer::get_token(&mut l);
            if l.token == '}' as i64 {
                printf!(c"    add rsp, %zu\n", vars_offset);
                printf!(c"    pop rbp\n", vars_offset);
                printf!(c"    mov rax, 0\n");
                printf!(c"    ret\n");
                break 'body;
            }
            if !expect_clex(&mut l, input_path, stb_c_lexer::CLEX::id as i64) { return 1; }
            if libc::strcmp(l.string, c"extrn".as_ptr()) == 0 {
                if !get_and_expect_clex(&mut l, input_path, stb_c_lexer::CLEX::id as i64) { return 1; }
                printf!(c"    extrn %s\n", l.string);
                // TODO: support multiple extrn declarations
                // TODO: report extrn redefinition
                if !get_and_expect_clex(&mut l, input_path, ';' as i64) { return 1; }
            } else if libc::strcmp(l.string, c"auto".as_ptr()) == 0 {
                if !get_and_expect_clex(&mut l, input_path, stb_c_lexer::CLEX::id as i64) { return 1; }
                vars_offset += 8;
                let name = libc::strdup(l.string);
                let name_where = l.where_firstchar;
                let existing_var = find_auto_var(ds::array_slice(vars), name);
                if !existing_var.is_null() {
                    let mut loc: stb_c_lexer::stb_lex_location = zeroed();
                    stb_c_lexer::get_location(&mut l, name_where, &mut loc);
                    fprintf!(libc::stderr, c"%s:%d:%d: ERROR: redefinition of variable `%s`\n", input_path, loc.line_number, loc.line_offset + 1, name);
                    stb_c_lexer::get_location(&mut l, (*existing_var).hwere, &mut loc);
                    fprintf!(libc::stderr, c"%s:%d:%d: NOTE: the first declaration is located here\n", input_path, loc.line_number, loc.line_offset + 1);
                    return 69;
                }
                ds::array_push(&mut vars, AutoVar {
                    name,
                    offset: vars_offset,
                    hwere: l.where_firstchar,
                });
                // TODO: support multiple auto declarations
                printf!(c"    sub rsp, 8\n");
                if !get_and_expect_clex(&mut l, input_path, ';' as i64) { return 1; }
            } else {
                let name = libc::strdup(l.string);
                let name_where = l.where_firstchar;

                stb_c_lexer::get_token(&mut l);
                if l.token == '=' as i64 {
                    let var_def = find_auto_var(ds::array_slice(vars), name);
                    if var_def.is_null() {
                        let mut loc: stb_c_lexer::stb_lex_location = zeroed();
                        stb_c_lexer::get_location(&mut l, name_where, &mut loc);
                        fprintf!(libc::stderr, c"%s:%d:%d: ERROR: could not find variable `%s`\n", input_path, loc.line_number, loc.line_offset + 1, name);
                        return 69;
                    }

                    // NOTE: expecting only int literal here for now
                    if !get_and_expect_clex(&mut l, input_path, stb_c_lexer::CLEX::intlit as i64) { return 1; }
                    printf!(c"    mov QWORD [rbp-%zu], %d\n", (*var_def).offset, l.int_number);
                    if !get_and_expect_clex(&mut l, input_path, ';' as i64) { return 1; }
                } else if l.token == '(' as i64 {
                    // NOTE: expecting only var read here for now

                    stb_c_lexer::get_token(&mut l);
                    if l.token == ')' as i64  {
                        // TODO: report calling unknown functions
                        printf!(c"    call %s\n", name);
                        if !get_and_expect_clex(&mut l, input_path, ';' as i64) { return 1; }
                    } else {
                        if !expect_clex(&mut l, input_path, stb_c_lexer::CLEX::id as i64) { return 1; }
                        let var_def = find_auto_var(ds::array_slice(vars), l.string);
                        if var_def.is_null() {
                            let mut loc: stb_c_lexer::stb_lex_location = zeroed();
                            stb_c_lexer::get_location(&mut l, l.where_firstchar, &mut loc);
                            fprintf!(libc::stderr, c"%s:%d:%d: ERROR: could not find variable `%s`\n", input_path, loc.line_number, loc.line_offset + 1, l.string);
                            return 69;
                        }

                        printf!(c"    mov rdi, [rbp-%zu]\n", (*var_def).offset);
                        printf!(c"    call %s\n", name);

                        if !get_and_expect_clex(&mut l, input_path, ')' as i64) { return 1; }
                        if !get_and_expect_clex(&mut l, input_path, ';' as i64) { return 1; }
                    }
                } else {
                    fprintf!(libc::stderr, c"TODO: stmt: report unexpected token\n");
                    libc::abort()
                }
            }
        }
    }
    0
}
