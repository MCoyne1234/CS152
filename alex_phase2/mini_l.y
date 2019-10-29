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


