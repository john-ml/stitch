all:
	bison -d parser.y
	flex lexer.l
	gcc -I util -g -lfl \
          lex.yy.c parser.tab.c ast.c \
          util/vec.c util/pair.c util/interning.c util/misc.c

clean:
	rm parser.tab* lex.yy.c

test: a.out
	valgrind --leak-check=full --show-leak-kinds=all ./a.out test_src
