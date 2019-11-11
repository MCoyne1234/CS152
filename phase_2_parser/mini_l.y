%{
#define YY_NO_UNPUT
#include <stdio.h>
#include <stdlib.h>
FILE * yyin;
void yyerror(const char* s);
%}

%union{
  char* ident_val;
  int num_val;
 }

%error-verbose
%start Program

%token <ident_val> IDENT
%token <num_val> NUMBER

%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token FOREACH
%token IN
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token READ
%token WRITE
%left AND
%left OR
%right NOT

%token TRUE
%token FALSE
%token RETURN

%left SUB
%left ADD
%left MULT
%left DIV
%left MOD
%left EQ
%left NEQ
%left LT
%left GT
%left LTE
%left GTE

%token COLON
%token SEMICOLON
%token COMMA
%left ASSIGN

%token PAREN_R
%token PAREN_L
%token SQUARE_BRACKET_R
%token SQUARE_BRACKET_L




%%  /*  Grammar rules and actions follow  */

Program:    %empty {printf("Program -> epsilon\n");} 
    | 
    Function Program {printf("Program -> Function Program\n");}
;

Function:   FUNCTION Ident SEMICOLON BEGIN_PARAMS Declarations END_PARAMS BEGIN_LOCALS Declarations END_LOCALS BEGIN_BODY Statements END_BODY
    {printf("Function -> FUNCTION Ident SEMICOLON BEGIN_PARAMS Declarations END_PARAMS BEGIN_LOCALS Declarations END_LOCALS BEGIN_BODY Statements END_BODY\n");}
;

Declaration:    Identifiers COLON INTEGER
    {printf("Declaration -> Identifiers COLON INTEGER\n");}
    | 
    Identifiers COLON ARRAY SQUARE_BRACKET_L NUMBER SQUARE_BRACKET_R OF INTEGER
		 {printf("Declaration -> Identifiers COLON ARRAY SQUARE_BRACKET_L NUMBER %d SQUARE_BRACKET_R OF INTEGER;\n", $5);}
;
Declarations:   %empty
    {printf("Declarations -> epsilon\n");}
    | Declaration SEMICOLON Declarations
    {printf("Declarations -> Declaration SEMICOLON Declarations\n");}
;

Identifiers:    Ident
    {printf("Identifiers -> Ident \n");}
        | Ident COMMA Identifiers
    {printf("Identifiers -> Ident COMMA Identifiers\n");}

Statements: Statement SEMICOLON Statements
    {printf("Statements -> Statement SEMICOLON Statements\n");}
        | Statement SEMICOLON
    {printf("Statements -> Statement SEMICOLON\n");}
;
Statement:  Var ASSIGN Expression
        {printf("Statement -> Var ASSIGN Expression\n");}
            | IF BoolExp THEN Statements ElseStatement ENDIF
        {printf("Statement -> IF BoolExp THEN Statements ElseStatement ENDIF\n");}		 
            | WHILE BoolExp BEGINLOOP Statements ENDLOOP
        {printf("Statement -> WHILE BoolExp BEGINLOOP Statements ENDLOOP\n");}
            | DO BEGINLOOP Statements ENDLOOP WHILE BoolExp
        {printf("Statement -> DO BEGINLOOP Statements ENDLOOP WHILE BoolExp\n");}
            | FOREACH Ident IN Ident BEGINLOOP Statements ENDLOOP
        {printf("Statement -> FOREACH Ident IN Ident BEGINLOOP Statements ENDLOOP\n");}
            | READ Vars
        {printf("Statement -> READ Vars\n");}
            | WRITE Vars
        {printf("Statement -> WRITE Vars\n");}
            | CONTINUE
        {printf("Statement -> CONTINUE\n");}
            | RETURN Expression
        {printf("Statement -> RETURN Expression\n");}
;
ElseStatement:   %empty
        {printf("ElseStatement -> epsilon\n");}
            | ELSE Statements
        {printf("ElseStatement -> ELSE Statements\n");}
;

Var:    Ident SQUARE_BRACKET_L Expression SQUARE_BRACKET_R
        {printf("Var -> Ident  SQUARE_BRACKET_L Expression SQUARE_BRACKET_R\n");}
            | Ident
        {printf("Var -> Ident \n");}
;
Vars:            Var
{printf("Vars -> Var\n");}
                 | Var COMMA Vars
		 {printf("Vars -> Var COMMA Vars\n");}
;

Expression:      MultExp
{printf("Expression -> MultExp\n");}
                 | MultExp ADD Expression
		 {printf("Expression -> MultExp ADD Expression\n");}
                 | MultExp SUB Expression
		 {printf("Expression -> MultExp SUB Expression\n");}
;
Expressions:     %empty
{printf("Expressions -> epsilon\n");}
                 | Expression COMMA Expressions
		 {printf("Expressions -> Expression COMMA Expressions\n");}
                 | Expression
		 {printf("Expressions -> Expression\n");}
;

MultExp:         Term
{printf("MultExp -> Term\n");}
                 | Term MULT MultExp
		 {printf("MultExp -> Term MULT MultExp\n");}
                 | Term DIV MultExp
		 {printf("MultExp -> Term DIV MultExp\n");}
                 | Term MOD MultExp
		 {printf("MultExp -> Term MOD MultExp\n");}
;

Term:            Var
{printf("Term -> Var\n");}
                 | SUB Var
		 {printf("Term -> SUB Var\n");}
                 | NUMBER
		 {printf("Term -> NUMBER %d\n", $1);}
                 | SUB NUMBER
		 {printf("Term -> SUB NUMBER %d\n", $2);}
                 | PAREN_L Expression PAREN_R
		 {printf("Term -> PAREN_L Expression PAREN_R\n");}
                 | SUB PAREN_L Expression PAREN_R
		 {printf("Term -> SUB PAREN_L Expression PAREN_R\n");}
                 | Ident PAREN_L Expressions PAREN_R
		 {printf("Term -> Ident PAREN_L Expressions PAREN_R\n");}
;

BoolExp:    RelAndExp 
{printf("bool_exp -> relation_exp\n");}
    | RelAndExp OR BoolExp
{printf("bool_exp -> relation_and_exp OR bool_exp\n");}
;

RelAndExp:           NRelExpr
{printf("relation_and_exp -> relation_exp\n");}
    | NRelExpr AND RelAndExp
{printf("relation_and_exp -> relation_exp AND relation_and_exp\n");}
;

NRelExpr:            NOT RelExpr 
{printf("relation_exp -> NOT relation_exp1\n");}
    | RelExpr
{printf("relation_exp -> relation_exp1\n");}
;

RelExpr:           Expression Comp Expression
    {printf("relation_exp -> Expression Comp Expression\n");}
        | TRUE
    {printf("relation_exp -> TRUE\n");}
        | FALSE
    {printf("relation_exp -> FALSE\n");}
        | PAREN_L BoolExp PAREN_R
    {printf("relation_exp -> PAREN_L BoolExp PAREN_R\n");}
;

Comp:   EQ
    {printf("comp -> EQ\n");}
        | NEQ
    {printf("comp -> NEQ\n");}
        | LT
    {printf("comp -> LT\n");}
        | GT
    {printf("comp -> GT\n");}
        | LTE
    {printf("comp -> LTE\n");}
        | GTE
    {printf("comp -> GTE\n");}
;

Ident:      IDENT
{printf("Ident -> IDENT %s \n", $1);}
%%

		 
int main(int argc, char **argv) {
   if (argc > 1) {
      yyin = fopen(argv[1], "r");
      if (yyin == NULL){
         printf("syntax: %s filename\n", argv[0]);
      }//end if
   }//end if
   yyparse(); // Calls yylex() for tokens.
   return 0;
}
		 
void yyerror(const char* s) {
  extern int currLine;
  extern char* yytext;

  printf("ERROR: %s at symbol \"%s\" on line %d\n", s, yytext, currLine);
  exit(1);
}
	
