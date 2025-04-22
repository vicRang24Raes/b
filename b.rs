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
        stb_c_lexer_get_location($l, $where, &mut loc);
        fprintf!(stderr, c"%s:%d:%d: ", $path, loc.line_number, loc.line_offset + 1);
        fprintf!(stderr, $fmt $($args)*);
    }};
}

macro_rules! todof {
    ($l:expr, $path:expr, $where:expr, $fmt:literal $($args:tt)*) => {{
        let file = file!();
        let mut loc: stb_lex_location = zeroed();
        stb_c_lexer_get_location($l, $where, &mut loc);
        fprintf!(stderr, c"%s:%d:%d: TODO: ", $path, loc.line_number, loc.line_offset + 1);
        fprintf!(stderr, $fmt $($args)*);
        fprintf!(stderr, c"%.*s:%d: INFO: implementation should go here\n", file.len(), file.as_ptr(), line!());
        abort();
    }}
}

unsafe fn display_token_kind_temp(token: c_long) -> *const c_char {
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
        CLEX_dqstring   => c"string literal".as_ptr(),
        // NOTE: single quote strings are opt-in in stb_c_lexer.h (see STB_C_LEX_C_SQ_STRINGS)
        CLEX_sqstring   => c"single quote literal".as_ptr(),
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
        diagf!(l, input_path, (*l).where_firstchar, c"ERROR: expected %s, but got %s\n", display_token_kind_temp(clex), display_token_kind_temp((*l).token));
        return false
    }
    true
}

unsafe fn get_and_expect_clex(l: *mut stb_lexer, input_path: *const c_char, clex: c_long) -> bool {
    stb_c_lexer_get_token(l);
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

#[derive(Clone, Copy)]
pub enum Op {
    AutoVar(usize),
    ExtrnVar(*const c_char),
    AutoAssign(usize, i64),
    Funcall {
        name: *const c_char,
        arg: Option<usize>,
    }
}

pub unsafe fn dump_ops(ops: *const [Op]) {
    for i in 0..ops.len() {
        match (*ops)[i] {
            Op::AutoVar(index) => {
                printf!(c"AutoVar(%zu)\n", index);
            },
            Op::ExtrnVar(name) => {
                printf!(c"ExtrnVar(\"%s\")\n", name);
            },
            Op::AutoAssign(index, value) => {
                printf!(c"AutoAssign(%zu, %ld)\n", index, value);
            },
            Op::Funcall{name, arg} => {
                match arg {
                    Some(arg) => {
                        printf!(c"Funcall(\"%s\", %ld)\n", name, arg);
                    }
                    None => {
                        printf!(c"Funcall(\"%s\")\n", name);
                    }
                }
            },
        }
    }
}

unsafe fn generate_fasm_x86_64_linux_executable(output: *mut String_Builder) {
    sb_appendf(output, c"format ELF64\n".as_ptr());
    sb_appendf(output, c"section \".text\" executable\n".as_ptr());
}

unsafe fn generate_fasm_x86_64_linux_func_prolog(name: *const c_char, output: *mut String_Builder) {
    sb_appendf(output, c"public %s\n".as_ptr(), name);
    sb_appendf(output, c"%s:\n".as_ptr(), name);
    sb_appendf(output, c"    push rbp\n".as_ptr());
    sb_appendf(output, c"    mov rbp, rsp\n".as_ptr());
}

unsafe fn generate_fasm_x86_64_linux_func_epilog(output: *mut String_Builder, vars_offset: usize) {
    sb_appendf(output, c"    add rsp, %zu\n".as_ptr(), vars_offset);
    sb_appendf(output, c"    pop rbp\n".as_ptr(), vars_offset);
    sb_appendf(output, c"    mov rax, 0\n".as_ptr());
    sb_appendf(output, c"    ret\n".as_ptr());
}

unsafe fn generate_fasm_x86_64_linux_func_body(body: *const [Op], output: *mut String_Builder) {
    for i in 0..body.len() {
        match (*body)[i] {
            Op::AutoVar(count) => {
                sb_appendf(output, c"    sub rsp, %zu\n".as_ptr(), count*8);
            },
            Op::ExtrnVar(name) => {
                sb_appendf(output, c"    extrn %s\n".as_ptr(), name);
            },
            Op::AutoAssign(index, value) => {
                sb_appendf(output, c"    mov QWORD [rbp-%zu], %d\n".as_ptr(), index*8, value);
            },
            Op::Funcall{name, arg} => {
                if let Some(index) = arg {
                    sb_appendf(output, c"    mov rdi, [rbp-%zu]\n".as_ptr(), index*8);
                }
                sb_appendf(output, c"    call %s\n".as_ptr(), name);
            },
        }
    }
}

unsafe fn compile_func_body(l: *mut stb_lexer, input_path: *const c_char, vars: *mut Array<Var>, vars_offset: *mut usize, func_body: *mut Array<Op>) -> bool {
    (*vars).count = 0;
    (*vars_offset) = 0;
    loop {
        // Statement
        stb_c_lexer_get_token(l);
        if (*l).token == '}' as c_long {
            return true;
        }
        if !expect_clex(l, input_path, CLEX_id) { return false; }
        if strcmp((*l).string, c"extrn".as_ptr()) == 0 {
            if !get_and_expect_clex(l, input_path, CLEX_id) { return false; }
            // TODO: support multiple extrn declarations

            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;
            let existing_var = find_var(array_slice(*vars), name);
            if !existing_var.is_null() {
                diagf!(l, input_path, name_where, c"ERROR: redefinition of variable `%s`\n", name);
                diagf!(l, input_path, (*existing_var).hwere, c"NOTE: the first declaration is located here\n");
                return false;
            }

            array_push(vars, Var {
                name,
                storage: Storage::External,
                offset: 0,  // Irrelevant for external variables
                hwere: (*l).where_firstchar,
            });

            array_push(func_body, Op::ExtrnVar(strdup((*l).string)));
            if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
        } else if strcmp((*l).string, c"auto".as_ptr()) == 0 {
            if !get_and_expect_clex(l, input_path, CLEX_id) { return false; }
            (*vars_offset) += 8;
            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;
            let existing_var = find_var(array_slice(*vars), name);
            if !existing_var.is_null() {
                diagf!(l, input_path, name_where, c"ERROR: redefinition of variable `%s`\n", name);
                diagf!(l, input_path, (*existing_var).hwere, c"NOTE: the first declaration is located here\n");
                return false;
            }
            array_push(vars, Var {
                name,
                storage: Storage::Auto,
                offset: (*vars_offset),
                hwere: (*l).where_firstchar,
            });
            // TODO: support multiple auto declarations
            array_push(func_body, Op::AutoVar(1));
            if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
        } else {
            let name = strdup((*l).string);
            let name_where = (*l).where_firstchar;

            stb_c_lexer_get_token(l);
            if (*l).token == '=' as c_long {
                let var_def = find_var(array_slice(*vars), name);
                if var_def.is_null() {
                    diagf!(l, input_path, name_where, c"ERROR: could not find variable `%s`\n", name);
                    return false;
                }

                // NOTE: expecting only int literal here for now
                if !get_and_expect_clex(l, input_path, CLEX_intlit) { return false; }
                match (*var_def).storage {
                    Storage::Auto => {
                        // TODO: store var_def.offset in words to prevent this /8 hack
                        array_push(func_body, Op::AutoAssign((*var_def).offset/8, (*l).int_number));
                    }
                    Storage::External => {
                        todof!(l, input_path, name_where, c"assignment to external variables\n");
                    }
                }

                if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
            } else if (*l).token == '(' as c_long {
                let var_def = find_var(array_slice(*vars), name);
                if var_def.is_null() {
                    diagf!(l, input_path, name_where, c"ERROR: could not find function `%s`\n", name);
                    return false;
                }

                stb_c_lexer_get_token(l);
                let mut arg = None;
                if (*l).token != ')' as c_long  {
                    // TODO: function calls with multiple arguments
                    // NOTE: expecting only var read here for now
                    if !expect_clex(l, input_path, CLEX_id) { return false; }
                    let var_def = find_var(array_slice(*vars), (*l).string);
                    if var_def.is_null() {
                        diagf!(l, input_path, (*l).where_firstchar, c"ERROR: could not find variable `%s`\n", (*l).string);
                        return false;
                    }
                    // TODO: store var_def.offset in words to prevent this /8 hack
                    arg = Some((*var_def).offset/8);
                    if !get_and_expect_clex(l, input_path, ')' as c_long) { return false; }
                }

                match (*var_def).storage {
                    Storage::External => {
                        array_push(func_body, Op::Funcall {name, arg});
                    }
                    Storage::Auto => {
                        todof!(l, input_path, name_where, c"calling functions from auto variables\n");
                    }
                }

                if !get_and_expect_clex(l, input_path, ';' as c_long) { return false; }
            } else {
                diagf!(l, input_path, (*l).where_firstchar, c"ERROR: unexpected token %s\n", display_token_kind_temp((*l).token));
                return false;
            }
        }
    }
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
    let mut vars_offset: usize = 0;
    let mut func_body: Array<Op> = zeroed();

    let mut input: String_Builder = zeroed();
    if !read_entire_file(input_path, &mut input) { return 1; }

    let mut l: stb_lexer    = zeroed();
    let mut string_store: [c_char; 1024] = zeroed(); // TODO: size of identifiers and string literals is limited because of stb_c_lexer.h
    stb_c_lexer_init(&mut l, input.items, input.items.add(input.count), string_store.as_mut_ptr(), string_store.len() as i32);

    let mut output: String_Builder = zeroed();
    generate_fasm_x86_64_linux_executable(&mut output);

    // TODO: are function also variables?
    //   Maybe some sort of global variables.
    'def: loop {
        stb_c_lexer_get_token(&mut l);
        if l.token == CLEX_eof { break 'def }

        if !expect_clex(&mut l, input_path, CLEX_id) { return 1; }

        let symbol_name = strdup(l.string);
        let symbol_name_where = l.where_firstchar;

        // TODO: maybe the keywords should be identified on the level of lexing
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

        stb_c_lexer_get_token(&mut l);
        if l.token == '(' as c_long { // Function definition
            // TODO: functions with several parameters
            if !get_and_expect_clex(&mut l, input_path, ')' as c_long) { return 1; }
            if !get_and_expect_clex(&mut l, input_path, '{' as c_long) { return 1; }

            generate_fasm_x86_64_linux_func_prolog(symbol_name, &mut output);
            if !compile_func_body(&mut l, input_path, &mut vars, &mut vars_offset, &mut func_body) { return 1; }
            generate_fasm_x86_64_linux_func_body(array_slice(func_body), &mut output);
            generate_fasm_x86_64_linux_func_epilog(&mut output, vars_offset);

            func_body.count = 0;
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
// TODO: Introduce some sort of IR
