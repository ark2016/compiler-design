%option reentrant noyywrap nounput noinput
%option bison-bridge bison-locations
%option prefix="l2"

%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "l2.tab.h"

/* Сопоставление типов для правильной работы с префиксом l2 */
typedef L2STYPE YYSTYPE;
typedef L2LTYPE YYLTYPE;

/* Получаем трекер памяти из сканера */
extern MemoryTracker* get_memory_tracker(void* scanner);

/* Макрос для сохранения текущей лексемы в yylval */
#define SAVE_TOKEN do { \
    if (get_memory_tracker(yyscanner)) { \
        yylval->string = tracked_strdup(get_memory_tracker(yyscanner), yytext); \
    } else { \
        yylval->string = strdup(yytext); \
    } \
} while(0)

/* Макрос для возврата токена с сохранением лексемы */
#define TOKEN(t) do { \
    SAVE_TOKEN; \
    return t; \
} while(0)

/* Счетчик строк и колонок */
static int column = 1;
static int line_len = 0;

/* Макрос для обновления позиции */
#define YY_USER_ACTION do { \
    yylloc->first_line = yylloc->last_line = yylineno; \
    yylloc->first_column = column; \
    yylloc->last_column = column + yyleng - 1; \
    column += yyleng; \
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
{NEWLINE}       { column = 1; line_len = 0; /* Игнорировать переводы строк */ }

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