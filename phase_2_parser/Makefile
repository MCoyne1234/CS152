CFLAGS = -g -Wall -ansi -pedantic

mini_l_x: mini_l.lex mini_l.y
	bison -d -v mini_l.y
	flex mini_l.lex	
	gcc lex.yy.c mini_l.tab.c -o mini_l_x
	rm -f lex.yy.c

clean:
	rm -f mini_l_x mini_l.tab.* mini_l.output *~ lex.yy.c 
