all:
	bison -d compiler/parser.y
	flex -o lexer.c --header-file=lexer.h compiler/lexer.l
	gcc -I util -I compiler \
          -g -lfl \
          -o stitch \
          lexer.c parser.tab.c \
          stitch.c \
          compiler/ast.c \
          util/vec.c util/pair.c util/interning.c util/misc.c

clean:
	rm parser.tab.* lexer.*

test: a.out
	valgrind --leak-check=full --show-leak-kinds=all ./stitch test_src
