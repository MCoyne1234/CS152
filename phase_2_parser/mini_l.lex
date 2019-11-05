   /* cs152-fall 2019 */
   /* A flex scanner specification for the mini language */
   /* Written by Matthew Coyne */
   
%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "mini_l.tab.h"
    int currLine = 1, currPos = 1;
%}   

DIGIT [0-9]+
IDENT [a-zA-Z]|([a-zA-Z][a-zA-Z0-9_]*[a-zA-Z0-9]+)
INVAL_N [0-9|_][a-zA-Z0-9|_]*
INVAL_U [a-zA-Z][a-zA-Z0-9_]*[_]+

%%
 /* ""  {printf("\n"); currPos += yyleng;} */
 // RESERVED KEYWORDS
"function" { currPos += yyleng; return FUNCTION;}
"beginparams" {currPos += yyleng; return BEGIN_PARAMS;}
"endparams" {currPos += yyleng; return END_PARAMS;}
"beginlocals" {currPos += yyleng; return BEGIN_LOCALS;}
"endlocals" {currPos += yyleng; return END_LOCALS;}
"beginbody" {currPos += yyleng; return BEGIN_BODY;}
"endbody" {currPos += yyleng; return END_BODY;}
"integer" {currPos += yyleng; return INTEGER;}
"array" {currPos += yyleng; return ARRAY;}
"of" {currPos += yyleng; return OF;}
"if" {currPos += yyleng; return IF;}
"then" {currPos += yyleng; return THEN;}
"endif" {currPos += yyleng; return ENDIF;}
"else" {currPos += yyleng; return ELSE;}
"while" {currPos += yyleng; return WHILE;}
"do" {currPos += yyleng; return DO;}
"beginloop" {currPos += yyleng; return BEGINLOOP;}
"endloop" {currPos += yyleng; return ENDLOOP;}
"continue" {currPos += yyleng; return CONTINUE;}
"read" {currPos += yyleng; return READ;}
"write" {currPos += yyleng; return WRITE;}
"and" {currPos += yyleng; return AND;}
"or" {currPos += yyleng; return OR;}
"not" {currPos += yyleng; return NOT;}
"true" {currPos += yyleng; return TRUE;}
"false" {currPos += yyleng; return FALSE;}
"currPos += yyleng; return" {currPos += yyleng; return RETURN;}
  /* ARITHMETIC OPERATORS */
"-"   {currPos += yyleng; return SUB;}
"+"   {currPos += yyleng; return ADD;}
"*"   {currPos += yyleng; return MULT;}
"/"   {currPos += yyleng; return DIV;}
"%"   {currPos += yyleng; return MOD;}
 /* COMPARISON OPERATORS */
"=="  {currPos += yyleng; return EQ;}
"<>"  {currPos += yyleng; return NEQ;}
"<"   {currPos += yyleng; return LT;}
">"   {currPos += yyleng; return GT;} 
"<="  {currPos += yyleng; return LTE;}
">="  {currPos += yyleng; return GTE;}
 /* OTHER SPECIAL SYMBOLS */
";"  {currPos += yyleng; return SEMICOLON;}
":"  {currPos += yyleng; return COLON;}
","  {currPos += yyleng; return COMMA;}
"("   {currPos += yyleng; return L_PAREN;}
")"   {currPos += yyleng; return R_PAREN;}
"["   {currPos += yyleng; return L_SQUARE_BRACKET;}
"]"   {currPos += yyleng; return R_SQUARE_BRACKET;}
":="  {currPos += yyleng; return ASSIGN;}

 /* HOUSEKEEPING */
"\n" {++currLine; currPos = 1;}
"\t" {++currPos;}
" "  {++currPos;}
##[^\n]* {currPos += yyleng;}
  
{DIGIT}  {currPos += yyleng; return NUMBER;}
{IDENT}  {currPos += yyleng; return IDENT;}
{INVAL_N}  {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n",currLine, currPos, yytext);currPos += yyleng;exit(0);}
{INVAL_U} {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n",currLine, currPos, yytext);currPos += yyleng;exit(0);}

.  {printf("Error at line %d, column %d: identifier \"%s\" unrecognized symbol\n",currLine, currPos, yytext);currPos += yyleng;exit(0);}

%%

yywrap(){}
  // int yyparse(); 
