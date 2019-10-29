%{
#include <stdio.h>
#include <stdlib.h>

	extern int currLine;
	extern int currPos;
	FILE * yyin;

	void yyerror (char const *msg)
	{
		fprintf(stderr, "%s at line %i\n", msg, lines);

	}
%}

%union
{
	double dval;
	int ival;
	char* name;
}

%error-verbose
%start program

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY
%token END_BODY INTEGER ARRAY OF IF  THEN ENDIF ELSE WHILE DO BEGINLOOP
%token ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN SUB ADD MULT
%token DIV MOD EQ NEQ LT GT LTE GTE SEMICOLON COLON COMMA L_PAREN R_PAREN
%token L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN

%token <dval> NUMBER
%token <dval> exp
%token <name> IDENT
%left PLUS MINUS
%left MULT DIV
%nonassoc UMINUS 

%%

program:	/*epsilon*/ {printf("program -> epsilon\n");}
		| function program {printf("program -> function\n\n");}
		;

function:	FUNCTION ident SEMICOLON params locals body {prinft("function -> FUNCTION ident SEMICOLON 
		params locals body\n");}
		| error
		;

params:         BEGIN_PARAMS declarations END_PARAMS
                    { printf("params -> BEGIN_PARAMS declarations END_PARAMS\n"); }
                | error 
                ;

locals:         BEGIN_LOCALS declarations END_LOCALS
                    { printf("local -> BEGIN_LOCALS declarations END_LOCALS\n"); }
                | error
                ;

body:           BEGIN_BODY stmts END_BODY
                    { printf("body -> BEGIN_BODY stmts END_BODY\n"); }
                | error
                ;

declarations:       { printf("declarations -> epsilon\n"); }
                | declaration SEMICOLON declarations
                    { printf("declarations -> declarations SEMI declarations\n"); }
                | declaration error
                ;

declaration:    idents COLON INTEGER
                    { printf("declaration -> idents COLON INTEGER\n"); }
                | idents COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
                { printf("declaration -> idents COLON ARRAY L_SQUARE NUMBER R_SQUARE OF INTEGER\n"); }
                | idents error
                ;

ident:          IDENT { printf("ident -> IDENT %s\n", $1); }

idents:         ident
                | ident COMMA idents
                    { printf("idents -> ident COMMA idents\n"); }
                ;
 
