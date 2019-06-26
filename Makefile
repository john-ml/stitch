all:
	bison -d parser.y
	flex lexer.l
	gcc -I util -g lex.yy.c parser.tab.c util/vec.c ast.c util/pair.c -lfl

clean:
	rm parser.tab* lex.yy.c

test: a.out
	valgrind --leak-check=full --show-leak-kinds=all ./a.out test_src
