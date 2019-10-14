   /* cs152-fall 2019 */
   /* A flex scanner specification for the mini language */
   /* Written by Matthew Coyne */
   
%{
    int currLine = 1, currPos = 1;
%}   

DIGIT [0-9]

"("   {printf("L_PAREN\n"); currPos += yyleng;}
")"   {printf("R_PAREN\n"); currPos += yyleng;}
"["   {}
"]"   {}
"-"   {}
"*"   {}
"/"   {}
"%"   {}
"+"   {}
"-"   {}
"<"   {}
"<="  {}
">"   {}
">="  {}
"=="  {}
"<>"  {}
"not" {}
"and" {}
"or"  {}
":="  {}
