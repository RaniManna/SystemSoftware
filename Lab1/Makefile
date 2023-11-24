Bison = win_bison
Flex = win_flex

all: bisonY91.tab.o lex.yy.o Ast.o Dfs.o
	gcc lex.yy.o bisonY91.tab.o Ast.o Dfs.o -o lab1

bisonY91.tab.o: bisonY91.tab.c
	gcc -c bisonY91.tab.c -o bisonY91.tab.o

lex.yy.o: lex.yy.c
	gcc -c lex.yy.c -o lex.yy.o

bisonY91.tab.c:
	$(Bison) -d  bisonY91.y

lex.yy.c:
	$(Flex) flexY91.l

ast.o:
	gcc -c ./tree/Ast.c -o Ast.o

dfs.o:
	gcc -c ./tree/Dfs.c -o Dfs.o

clear:
	rm -f bisonY91.tab.h bisonY91.tab.c lex.yy.c bisonY91.tab.o lex.yy.o Ast.o Dfs.o bison.gv out.svg lab1

