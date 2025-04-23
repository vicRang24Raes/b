.PHONY: examples
examples: hello.js hello

hello.js: hello.b b
	./b hello.b -o hello.js -target js

hello: hello.o
	clang -no-pie -o hello hello.o

hello.o: hello.asm
	fasm hello.asm hello.o

hello.asm: hello.b b
	./b hello.b -o hello.asm

b: b.rs libc.rs crust.rs nob.rs stb_c_lexer.rs nob.o stb_c_lexer.o flag.o
	rustc --edition 2021 -g -C opt-level=z -C link-args="-lc nob.o stb_c_lexer.o flag.o" -C panic="abort" b.rs

nob.o: nob.h
	clang -g -x c -DNOB_IMPLEMENTATION -c nob.h

stb_c_lexer.o: stb_c_lexer.h
	clang -g -x c -DSTB_C_LEXER_IMPLEMENTATION -c stb_c_lexer.h

flag.o: flag.h
	clang -g -x c -DFLAG_IMPLEMENTATION -c flag.h

# TODO: use nob to build the project
