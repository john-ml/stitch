all:
	bison -d parser.y
	flex lexer.l
	gcc -g lex.yy.c parser.tab.c vec.c ast.c pair.c -lfl

clean:
	rm parser.tab* lex.yy.c

test:
	valgrind --leak-check=full --show-leak-kinds=all ./a.out test_src
