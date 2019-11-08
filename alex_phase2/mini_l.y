%{
	#include <stdio.h>
	#include <stdlib.h>

	void yyerror(const char *msg);

	extern int currLine;
	extern int currPos;
	
	FILE * yyin;
 
%}


%union
{
	double dval;
	int ival;
	char* name;
}

%error-verbose

%left ADD SUB
%left MULT DIV
%start program

%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY
%token INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO BEGINLOOP ENDLOOP CONTINUE READ
%token WRITE AND OR NOT TRUE FALSE RETURN SUB ADD MULT DIV MOD EQ NEQ LT GT LTE GTE
%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN
%token <name> NUMBER
%token <name> IDENT

%nonassoc UMINUS






%%

program :   functions {printf("program -> functions\n\n");}
	;

functions : /*epsilon*/ {printf("functions -> epsilon\n");}
	  | function functions {printf("functions -> function functions\n");}
	  ;


function : FUNCTION ident SEMICOLON BEGIN_PARAMS param_declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY {printf("function -> FUNCTION ident SEMICOLON BEGIN_PARAMS param_declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY\n");}
	 |error 
 	 ;

param_declarations : /*epsilon*/ {printf("param_declarations -> epsilon\n");}
     | declaration SEMICOLON param_declarations {printf("param_declarations -> declaration SEMICOLON param_declarations\n");}
     | error 
     ;

declarations      : /*epsilon*/ {printf("declarations -> epsilon\n");}
       | declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declarations\n");}
       | error 
       ;  


declaration : identifiers COLON array INTEGER {printf("declaration -> identifiers COLON array INTEGER\n");}    
            | identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}            
            ;


identifiers : ident {printf("identifiers -> ident\n");}
	    | ident COMMA identifiers {printf("identifiers -> ident COMMA identifiers\n");}
	    ;

ident : IDENT {printf("ident -> IDENT %s\n",$1);}
      ;

array  : ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF {printf("array -> ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF\n");} 
      ;

statements : /*epsilon*/ {printf("statements -> epsilon\n");}
  | statement SEMICOLON statements {printf("statements -> statement SEMICOLON statements\n");}
  | error
  ;
  

vars : var {printf("vars -> var\n");}
  | var COMMA vars {printf("vars -> var COMMA vars\n");}
  ;

bool_expression : relation_and_expression {printf("bool_expression -> relation_and_expression\n");}
   | bool_expression OR relation_and_expression {printf("bool_expression -> bool_expression OR relation_and_expression\n");}
   ;

relation_and_expression : relation_expression {printf("relation_and_expression -> relation_expression\n");}
    | relation_and_expression AND relation_expression {printf("relation_and_expression -> relation_and_expression AND relation_expression\n");}
    ;

relation_expression : NOT relation_expression {printf("relation_expression -> NOT relation_expression\n");}
   | expression comp expression {printf("relation_expression -> expression comp expression\n");}
   | TRUE {printf("relation_expression -> TRUE\n");} 
   | FALSE {printf("relation_expression -> FALSE\n");}
   | L_PAREN bool_expression R_PAREN {printf("relation_expression -> L_PAREN bool_expression R_PAREN\n");}
   ;

comp : EQ {printf("comp -> EQ\n");}
     | NEQ {printf("comp -> NEQ\n");}
     | LT {printf("comp -> LT\n");}
     | GT {printf("comp -> GT\n");}
     | LTE {printf("comp -> LTE\n");}
     | GTE {printf("comp -> GTE\n");}
     | error 
     ;
     




statement   : var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
	    | IF bool_expression THEN statements ENDIF    {printf("statement -> IF bool_expression THEN statements ENDIF\n");}
	    | IF bool_expression THEN statements ELSE statements ENDIF {printf("statement -> IF bool_expression THEN statements ELSE statements ENDIF\n");}
	    | WHILE bool_expression BEGINLOOP statements ENDLOOP {printf("statement -> WHILE bool_expression BEGINLOOP statements ENDLOOP\n");}
	  | DO BEGINLOOP statements ENDLOOP WHILE bool_expression {printf("statement -> DO BEGINLOOP statements ENDLOOP WHILE bool_expression\n");}
	  | READ vars {printf("statement -> READ vars\n");}
	  | WRITE vars {printf("statement -> WRITE vars\n");}
	  | CONTINUE {printf("statement -> CONTINUE\n");}
	  | RETURN expression {printf("statement -> RETURN expression\n");}
	  ;

expression : multiplicative_expression {printf("expression -> multiplicative_expression\n");}
	   | expression ADD multiplicative_expression {printf("expression -> expression ADD multiplicative_expression\n");}
	   | expression SUB multiplicative_expression {printf("expression -> expression SUB multiplicative_expression\n");}
	   | error	  
           ;

multiplicative_expression : multiplicative_expression MULT term {printf("multiplicative_expression -> multiplicative_expression MULT term\n");}
		| multiplicative_expression DIV term  {printf("multiplicative_expression -> multiplicative_expression DIV term\n");}
		| multiplicative_expression MOD term   {printf("multiplicative_expression -> multiplicative_expression MOD term\n");}
		| term   {printf("multiplicative_expression -> term\n");}
		;

expression_comma : expression {printf("expression_comma -> expression\n");}
         | expression COMMA expression_comma {printf("expression_comma -> expression COMMA expression_comma\n");}  
         ; 

term : SUB var                          {printf("term -> SUB var\n");}
     | SUB NUMBER                       {printf("term -> SUB NUMBER\n");}
     | SUB L_PAREN expression R_PAREN   {printf("term -> SUB L_PAREN expression R_PAREN\n");}
     | var                              {printf("term -> var\n");}
     | NUMBER                           {printf("term -> NUMBER\n");}
     | L_PAREN expression R_PAREN       {printf("term -> L_PAREN expression R_PAREN\n");}
     | IDENT L_PAREN R_PAREN            {printf("term -> IDENT L_PAREN R_PAREN\n");}
     | IDENT L_PAREN expression_comma R_PAREN   {printf("term -> IDENT L_PAREN expression_comma R_PAREN\n");}
     ;
var : IDENT                             {printf("var -> IDENT %s\n",$1);}
    | IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
    ;

%%

int main(int argc, char **argv)
{	
	yyparse();
	return 0;

}

void yyerror (char const *msg)
{
	fprintf (stderr, "%s at line %i\n", msg, currLine);
}

        


