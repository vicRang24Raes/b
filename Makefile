main: main.o
	clang -no-pie -o main main.o

main.o: main.asm
	fasm main.asm main.o

main.asm: main.b b
	./b main.b | tee main.asm

b: b.o nob.o stb_c_lexer.o
	clang -g -o b b.o nob.o stb_c_lexer.o

b.o: b.rs
	rustc --edition 2021 -g -C opt-level=z --emit=obj -C panic="abort" b.rs

nob.o: nob.h
	clang -g -x c -DNOB_IMPLEMENTATION -c nob.h

stb_c_lexer.o: stb_c_lexer.h
	clang -g -x c -DSTB_C_LEXER_IMPLEMENTATION -c stb_c_lexer.h

# TODO: use nob to build the project
