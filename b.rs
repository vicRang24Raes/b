#![no_main]
#![no_std]
#![allow(non_upper_case_globals)]
extern crate core;

#[panic_handler]
unsafe fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[macro_use]
pub mod libc;
#[macro_use]
pub mod crust;
#[macro_use]
pub mod nob;
pub mod stb_c_lexer;

use core::panic::PanicInfo;
use core::ffi::*;
use core::mem::zeroed;
use core::ptr;
use libc::*;
use crust::*;
use nob::*;
use stb_c_lexer::*;

macro_rules! diagf {
    ($l:expr, $path:expr, $where:expr, $fmt:literal $($args:tt)*) => {{
        let mut loc: stb_lex_location = zeroed();
        get_location($l, $where, &mut loc);
        fprintf!(stderr, c"%s:%d:%d: ", $path, loc.line_number, loc.line_offset + 1);
        fprintf!(stderr, $fmt $($args)*);
    }};
}

macro_rules! todof {
    ($l:expr, $path:expr, $where:expr, $fmt:literal $($args:tt)*) => {{
        let file = file!();
        let mut loc: stb_lex_location = zeroed();
        get_location($l, $where, &mut loc);
        fprintf!(stderr, c"%s:%d:%d: TODO: ", $path, loc.line_number, loc.line_offset + 1);
        fprintf!(stderr, $fmt $($args)*);
        fprintf!(stderr, c"%.*s:%d: INFO: implementation should go here\n", file.len(), file.as_ptr(), line!());
        abort();
    }}
}

unsafe fn display_token_temp(token: c_long) -> *const c_char {
    match token {
        CLEX_id         => c"identifier".as_ptr(),
        CLEX_eq         => c"==".as_ptr(),
        CLEX_noteq      => c"!=".as_ptr(),
        CLEX_lesseq     => c"<=".as_ptr(),
        CLEX_greatereq  => c">=".as_ptr(),
        CLEX_andand     => c"&&".as_ptr(),
        CLEX_oror       => c"||".as_ptr(),
        CLEX_shl        => c"<<".as_ptr(),
        CLEX_shr        => c">>".as_ptr(),
        CLEX_plusplus   => c"++".as_ptr(),
        CLEX_minusminus => c"--".as_ptr(),
        CLEX_arrow      => c"->".as_ptr(),
        CLEX_andeq      => c"&=".as_ptr(),
        CLEX_oreq       => c"|=".as_ptr(),
        CLEX_xoreq      => c"^=".as_ptr(),
        CLEX_pluseq     => c"+=".as_ptr(),
        CLEX_minuseq    => c"-=".as_ptr(),
        CLEX_muleq      => c"*=".as_ptr(),
        CLEX_diveq      => c"/=".as_ptr(),
        CLEX_modeq      => c"%%=".as_ptr(),
        CLEX_shleq      => c"<<=".as_ptr(),
        CLEX_shreq      => c">>=".as_ptr(),
        CLEX_eqarrow    => c"=>".as_ptr(),
        // TODO: display_token_temp doesn't escape quoted literals
        CLEX_dqstring   => c"string literal".as_ptr(),
        CLEX_sqstring   => c"single quote literal".as_ptr(), // TODO: How are those different from CLEX_charlit?
        CLEX_charlit    => c"character literal".as_ptr(),
        CLEX_intlit     => c"integer literal".as_ptr(),
        CLEX_floatlit   => c"floating-point literal".as_ptr(),
        _ => {
            if token >= 0 && token < 256 {
                temp_sprintf(c"`%c`".as_ptr(), token)
            } else {
                temp_sprintf(c"<<<UNKNOWN TOKEN %ld>>>".as_ptr(), token)
            }
        }
    }
}

unsafe fn expect_clex(l: *const stb_lexer, input_path: *const c_char, clex: i64) -> bool {
    if (*l).token != clex {
        diagf!(l, input_path, (*l).where_firstchar, c"ERROR: expected %s, but got %s\n", display_token_temp(clex), display_token_temp((*l).token));
        return false
    }
    true
}

unsafe fn get_and_expect_clex(l: *mut stb_lexer, input_path: *const c_char, clex: c_long) -> bool {
    get_token(l);
    expect_clex(l, input_path, clex)
}

#[repr(C)]
#[derive(Clone, Copy)]
enum Storage {
    External,
    Auto
}

#[derive(Clone, Copy)]
struct Var {
    name: *const c_char,
    offset: usize,
    hwere: *mut c_char,
    storage: Storage,
}

unsafe fn find_var(vars: *mut [Var], name: *const c_char) -> *mut Var {
    for i in 0..vars.len() {
        if strcmp((*vars)[i].name, name) == 0 {
            return &mut (*vars)[i];
        }
    }
    return ptr::null_mut();
}

unsafe fn usage(program_name: *const c_char) {
    fprintf!(stderr, c"Usage: %s <input.b> <output.asm>\n", program_name);
}

const B_KEYWORDS: *const [*const c_char] = &[
    c"auto".as_ptr(),
    c"extrn".as_ptr(),
    c"case".as_ptr(),
    c"if".as_ptr(),
    c"while".as_ptr(),
    c"switch".as_ptr(),
    c"goto".as_ptr(),
    c"return".as_ptr(),
];

unsafe fn is_keyword(name: *const c_char) -> bool {
    for i in 0..B_KEYWORDS.len() {
        if strcmp((*B_KEYWORDS)[i], name) == 0 {
            return true
        }
    }
    false
}

#[no_mangle]
unsafe extern "C" fn main(mut _argc: i32, mut _argv: *mut *mut c_char) -> i32 {
    let program_name = shift!(_argv, _argc);

    if _argc <= 0 {
        usage(program_name);
        fprintf!(stderr, c"ERROR: no input is provided\n");
        return 69;
    }
    let input_path = shift!(_argv, _argc);

    if _argc <= 0 {
        usage(program_name);
        fprintf!(stderr, c"ERROR: no output is provided\n");
        return 69;
    }
    let output_path = shift!(_argv, _argc);

    let mut vars: Array<Var> = zeroed();
    let mut vars_offset: usize;

    let mut input: String_Builder = zeroed();
    if !read_entire_file(input_path, &mut input) { return 1; }

    let mut l: stb_lexer    = zeroed();
    let mut string_store: [c_char; 1024] = zeroed(); // TODO: size of identifiers and string literals is limited because of stb_c_lexer.h
    init(&mut l, input.items, input.items.add(input.count), string_store.as_mut_ptr(), string_store.len() as i32);

    let mut output: String_Builder = zeroed();
    sb_appendf(&mut output, c"format ELF64\n".as_ptr());
    sb_appendf(&mut output, c"section \".text\" executable\n".as_ptr());

    // TODO: are function also variables?
    //   Maybe some sort of global variables.
    'def: loop {
        vars.count = 0;
        vars_offset = 0;

        get_token(&mut l);
        if l.token == CLEX_eof { break 'def }

        if !expect_clex(&mut l, input_path, CLEX_id) { return 1; }

        let symbol_name = strdup(l.string);
        let symbol_name_where = l.where_firstchar;

        if is_keyword(l.string) {
            diagf!(&l, input_path, symbol_name_where, c"ERROR: Trying to define a reserved keyword `%s` as a symbol. Please choose a different name.\n", symbol_name);
            diagf!(&l, input_path, symbol_name_where, c"NOTE: Reserved keywords are: ");
            for i in 0..B_KEYWORDS.len() {
                if i > 0 {
                    fprintf!(stderr, c", ");
                }
                fprintf!(stderr, c"`%s`", (*B_KEYWORDS)[i]);
            }
            fprintf!(stderr, c"\n");
            return 69;
        }

        get_token(&mut l);
        if l.token == '(' as c_long { // Function definition
            sb_appendf(&mut output, c"public %s\n".as_ptr(), symbol_name);
            sb_appendf(&mut output, c"%s:\n".as_ptr(), symbol_name);
            sb_appendf(&mut output, c"    push rbp\n".as_ptr());
            sb_appendf(&mut output, c"    mov rbp, rsp\n".as_ptr());

            // TODO: functions with several parameters
            if !get_and_expect_clex(&mut l, input_path, ')' as c_long) { return 1; }
            if !get_and_expect_clex(&mut l, input_path, '{' as c_long) { return 1; }

            'body: loop {
                // Statement
                get_token(&mut l);
                if l.token == '}' as c_long {
                    sb_appendf(&mut output, c"    add rsp, %zu\n".as_ptr(), vars_offset);
                    sb_appendf(&mut output, c"    pop rbp\n".as_ptr(), vars_offset);
                    sb_appendf(&mut output, c"    mov rax, 0\n".as_ptr());
                    sb_appendf(&mut output, c"    ret\n".as_ptr());
                    break 'body;
                }
                if !expect_clex(&mut l, input_path, CLEX_id) { return 1; }
                if strcmp(l.string, c"extrn".as_ptr()) == 0 {
                    if !get_and_expect_clex(&mut l, input_path, CLEX_id) { return 1; }
                    // TODO: support multiple extrn declarations

                    let name = strdup(l.string);
                    let name_where = l.where_firstchar;
                    let existing_var = find_var(array_slice(vars), name);
                    if !existing_var.is_null() {
                        diagf!(&l, input_path, name_where, c"ERROR: redefinition of variable `%s`\n", name);
                        diagf!(&l, input_path, (*existing_var).hwere, c"NOTE: the first declaration is located here\n");
                        return 69;
                    }

                    array_push(&mut vars, Var {
                        name,
                        storage: Storage::External,
                        offset: 0,  // Irrelevant for external variables
                        hwere: l.where_firstchar,
                    });

                    sb_appendf(&mut output, c"    extrn %s\n".as_ptr(), l.string);
                    if !get_and_expect_clex(&mut l, input_path, ';' as c_long) { return 1; }
                } else if strcmp(l.string, c"auto".as_ptr()) == 0 {
                    if !get_and_expect_clex(&mut l, input_path, CLEX_id) { return 1; }
                    vars_offset += 8;
                    let name = strdup(l.string);
                    let name_where = l.where_firstchar;
                    let existing_var = find_var(array_slice(vars), name);
                    if !existing_var.is_null() {
                        diagf!(&l, input_path, name_where, c"ERROR: redefinition of variable `%s`\n", name);
                        diagf!(&l, input_path, (*existing_var).hwere, c"NOTE: the first declaration is located here\n");
                        return 69;
                    }
                    array_push(&mut vars, Var {
                        name,
                        storage: Storage::Auto,
                        offset: vars_offset,
                        hwere: l.where_firstchar,
                    });
                    // TODO: support multiple auto declarations
                    sb_appendf(&mut output, c"    sub rsp, 8\n".as_ptr());
                    if !get_and_expect_clex(&mut l, input_path, ';' as c_long) { return 1; }
                } else {
                    let name = strdup(l.string);
                    let name_where = l.where_firstchar;

                    get_token(&mut l);
                    if l.token == '=' as c_long {
                        let var_def = find_var(array_slice(vars), name);
                        if var_def.is_null() {
                            diagf!(&l, input_path, name_where, c"ERROR: could not find variable `%s`\n", name);
                            return 69;
                        }

                        // NOTE: expecting only int literal here for now
                        if !get_and_expect_clex(&mut l, input_path, CLEX_intlit) { return 1; }
                        match (*var_def).storage {
                            Storage::Auto => {
                                sb_appendf(&mut output, c"    mov QWORD [rbp-%zu], %d\n".as_ptr(), (*var_def).offset, l.int_number);
                            }
                            Storage::External => {
                                todof!(&l, input_path, name_where, c"assignment to external variables\n");
                            }
                        }

                        if !get_and_expect_clex(&mut l, input_path, ';' as c_long) { return 1; }
                    } else if l.token == '(' as c_long {
                        let var_def = find_var(array_slice(vars), name);
                        if var_def.is_null() {
                            diagf!(&l, input_path, name_where, c"ERROR: could not find function `%s`\n", name);
                            return 69;
                        }

                        get_token(&mut l);
                        if l.token != ')' as c_long  {
                            // TODO: function calls with multiple arguments
                            // NOTE: expecting only var read here for now
                            if !expect_clex(&mut l, input_path, CLEX_id) { return 1; }
                            let var_def = find_var(array_slice(vars), l.string);
                            if var_def.is_null() {
                                diagf!(&l, input_path, l.where_firstchar, c"ERROR: could not find variable `%s`\n", l.string);
                                return 69;
                            }

                            sb_appendf(&mut output, c"    mov rdi, [rbp-%zu]\n".as_ptr(), (*var_def).offset);
                            if !get_and_expect_clex(&mut l, input_path, ')' as c_long) { return 1; }
                        }

                        match (*var_def).storage {
                            Storage::External => {
                                sb_appendf(&mut output, c"    call %s\n".as_ptr(), name);
                            }
                            Storage::Auto => {
                                todof!(&l, input_path, name_where, c"calling functions from auto variables\n");
                            }
                        }

                        if !get_and_expect_clex(&mut l, input_path, ';' as c_long) { return 1; }
                    } else {
                        diagf!(&l, input_path, l.where_firstchar, c"ERROR: unexpected token %s\n", display_token_temp(l.token));
                        return 69;
                    }
                }
            }
        } else { // Variable definition
            todof!(&l, input_path, l.where_firstchar, c"variable definitions\n");
        }
    }
    if !write_entire_file(output_path, output.items as *const c_void, output.count) { return 69 }
    0
}

// TODO: B lexing is different from the C one.
//   Hack stb_c_lexer.h into stb_b_lexer.h
// TODO: Create a roadmap based on the spec.
