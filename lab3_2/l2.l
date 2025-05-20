%option reentrant noyywrap nounput noinput
%option bison-bridge
%option prefix="l2"

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "l2.tab.h"

/* Макрос для сохранения текущей лексемы в yylval */
#define SAVE_TOKEN do { \
    yylval->string = strdup(yytext); \
} while(0)

/* Макрос для возврата токена с сохранением лексемы */
#define TOKEN(t) do { \
    SAVE_TOKEN; \
    return t; \
} while(0)

/* Счетчик строк и колонок */
static int yycolumn = 1;
static int line_len = 0;

/* Макрос для обновления позиции */
#define YY_USER_ACTION do { \
    yylloc->first_line = yylloc->last_line = yylineno; \
    yylloc->first_column = yycolumn; \
    yylloc->last_column = yycolumn + yyleng - 1; \
    yycolumn += yyleng; \
    line_len += yyleng; \
} while(0);

%}

%option yylineno

/* Регулярные выражения для лексем */
DIGIT       [0-9]
LETTER      [a-zA-Zа-яА-Я]
WHITESPACE  [ \t]
NEWLINE     \r|\n|\r\n
IDENTIFIER  \{[^\}]*\}

%%

{WHITESPACE}+   { /* игнорировать пробелы */ }
{NEWLINE}       { yycolumn = 1; line_len = 0; return NEWLINE; }

"##".*          { /* однострочный комментарий */ TOKEN(COMMENT); }
"#("            { /* начало многострочного комментария */ TOKEN(COMMENT); }
")#"            { /* конец многострочного комментария */ TOKEN(COMMENT); }

"bool"          { TOKEN(BOOL); }
"char"          { TOKEN(CHAR); }
"int"           { TOKEN(INT); }
"void"          { TOKEN(VOID); }
"return"        { TOKEN(RETURN); }
"loop"          { TOKEN(LOOP); }
"while"         { TOKEN(WHILE); }
"then"          { TOKEN(THEN); }
"else"          { TOKEN(ELSE); }
"null"          { TOKEN(NULL_LIT); }
"true"          { TOKEN(TRUE_LIT); }
"false"         { TOKEN(FALSE_LIT); }

{IDENTIFIER}    { TOKEN(IDENTIFIER); }

{DIGIT}+        { TOKEN(INT_CONST); }
\"[^\"]*\"      { TOKEN(STRING_CONST); }
\$\"[^\"]*\"    { TOKEN(CHAR_CONST); }

":="            { TOKEN(ASSIGN); }
"<-"            { TOKEN(CALL_OP); }
"=="            { TOKEN(EQ); }
"!="            { TOKEN(NEQ); }
"<="            { TOKEN(LEQ); }
">="            { TOKEN(GEQ); }
"&"             { TOKEN(AND); }
"|"             { TOKEN(OR); }
"@"             { TOKEN(XOR); }
"!"             { TOKEN(NOT); }
"+"             { TOKEN(PLUS); }
"-"             { TOKEN(MINUS); }
"*"             { TOKEN(MUL); }
"/"             { TOKEN(DIV); }
"%"             { TOKEN(MOD); }
"^"             { TOKEN(POW); }
"="             { TOKEN(EQUAL); }

";"             { TOKEN(SEMICOLON); }
","             { TOKEN(COMMA); }
"."             { TOKEN(DOT); }
"["             { TOKEN(LBRACKET); }
"]"             { TOKEN(RBRACKET); }
"("             { TOKEN(LPAREN); }
")"             { TOKEN(RPAREN); }
"<"             { TOKEN(LT); }
">"             { TOKEN(GT); }
"~"             { TOKEN(TILDE); }

.               { printf("Неизвестный символ %c на строке %d\n", *yytext, yylineno); return UNKNOWN; }

%% 