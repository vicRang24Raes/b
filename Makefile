hello.js: hello.b b
	./b hello.b -o hello.js -target js

hello: hello.o
	clang -no-pie -o hello hello.o

hello.o: hello.asm
	fasm hello.asm hello.o

hello.asm: hello.b b
	./b hello.b -o hello.asm

b: b.o nob.o stb_c_lexer.o
	clang -g -o b b.o nob.o stb_c_lexer.o

b.o: b.rs libc.rs crust.rs nob.rs stb_c_lexer.rs
	rustc --edition 2021 -g -C opt-level=z --emit=obj -C panic="abort" b.rs

nob.o: nob.h
	clang -g -x c -DNOB_IMPLEMENTATION -c nob.h

stb_c_lexer.o: stb_c_lexer.h
	clang -g -x c -DSTB_C_LEXER_IMPLEMENTATION -c stb_c_lexer.h

# TODO: use nob to build the project
