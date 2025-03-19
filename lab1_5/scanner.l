%{
/* scanner.l - Лексический анализатор для:
   - Чисел фибоначчиевой системы счисления (последовательность '0' и '1',
     при условии, что две '1' подряд недопустимы).
   - Идентификаторов: последовательностей букв, где символы чередуются по типу
     (гласная, согласная, гласная и т.д.).
   Также обрабатываются C-комментарии (начало "/ *" и конец "* /").
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#define TAG_IDENT "IDENT"
#define TAG_FIB   "FIB_NUM"

static int line = 1, col = 1;
static int token_start_line, token_start_col, token_end_line, token_end_col;

/* Функция для вывода токена с координатами и атрибутом (если есть) */
void print_token(const char *tag, long long attr, int has_attr) {
    if (has_attr)
        printf("%s (%d,%d)-(%d,%d): %lld\n", tag, token_start_line, token_start_col,
               token_end_line, token_end_col, attr);
    else
        printf("%s (%d,%d)-(%d,%d):\n", tag, token_start_line, token_start_col,
               token_end_line, token_end_col);
}

/* Таблица идентификаторов */
#define MAX_IDENTIFIERS 1000
typedef struct {
    char *name;
    int index;
} IdentEntry;
static IdentEntry idents[MAX_IDENTIFIERS];
static int ident_count = 0;
int add_identifier(const char *name) {
    for (int i = 0; i < ident_count; i++) {
        if (strcmp(idents[i].name, name) == 0)
            return idents[i].index;
    }
    if (ident_count >= MAX_IDENTIFIERS) {
         fprintf(stderr, "Error: Identifier table overflow\n");
         exit(1);
    }
    idents[ident_count].name = strdup(name);
    idents[ident_count].index = ident_count;
    ident_count++;
    return ident_count - 1;
}

/* Проверка, является ли символ гласной (без учета регистра) */
int is_vowel(char c) {
    c = tolower(c);
    return (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u');
}

/* Вычисление числового значения числа, записанного в фибоначчиевой системе.
   Правая цифра соответствует F(2)=1, затем F(3)=2, F(4)=3, F(5)=5 и т.д. */
long long fib_value(const char *s, int len) {
    long long *fib = malloc((len + 2) * sizeof(long long));
    if (!fib) { perror("malloc"); exit(1); }
    fib[2] = 1;
    if (len + 1 >= 3)
        fib[3] = 2;
    for (int i = 4; i <= len + 1; i++) {
         fib[i] = fib[i - 1] + fib[i - 2];
    }
    long long value = 0;
    for (int i = 0; i < len; i++) {
         if (s[i] == '1')
              value += fib[len + 1 - i];
    }
    free(fib);
    return value;
}

/* Макрос для обновления координат токена */
#define YY_USER_ACTION \
    { \
       token_start_line = line; \
       token_start_col = col; \
       for (int i = 0; i < yyleng; i++) { \
           if (yytext[i] == '\n') { line++; col = 1; } else { col++; } \
       } \
       token_end_line = line; \
       token_end_col = col; \
    }

/* Массив для хранения ошибок */
#define MAX_ERRORS 1000
static char* errors[MAX_ERRORS];
static int error_count = 0;

/* Функция для добавления ошибки в массив */
void add_error(const char *format, ...) {
    if (error_count < MAX_ERRORS - 1) {
        char buffer[256];
        va_list args;
        va_start(args, format);
        vsnprintf(buffer, sizeof(buffer), format, args);
        va_end(args);
        errors[error_count] = strdup(buffer);
        error_count++;
    } else if (error_count == MAX_ERRORS - 1) {
        errors[error_count] = "Too many errors\n";
        error_count++;
    }
}

/* Функция для вывода всех собранных ошибок */
void report_errors() {
    if (error_count > 0) {
         printf("Errors encountered:\n");
         for (int i = 0; i < error_count; i++) {
              printf("%s\n", errors[i]);
              free(errors[i]);
         }
    }
}

%}

%option noyywrap

/* Определения регулярных выражений */
DIGIT    [0-9]
LETTER   [A-Za-z]
IDENT    {LETTER}+
FIB      [01]+

%x COMMENT

%%

[ \t\n]+               ;

"/*"                   { BEGIN(COMMENT); }
<COMMENT>[^*]+        { /* Пропускаем содержимое комментария */ }
<COMMENT>"*/"         { BEGIN(INITIAL); }
<COMMENT>\*+          { /* Пропускаем последовательности '*' */ }
<COMMENT><<EOF>>      { add_error("Error: Unclosed comment starting at (%d,%d)", token_start_line, token_start_col); BEGIN(INITIAL); }

{FIB} {
    int valid = 1;
    for (int i = 0; i < yyleng - 1; i++) {
         if (yytext[i] == '1' && yytext[i+1] == '1') { valid = 0; break; }
    }
    if (!valid) {
         add_error("Error: Invalid Fibonacci number (two consecutive '1's) at (%d,%d)-(%d,%d)", 
             token_start_line, token_start_col, token_end_line, token_end_col);
    } else {
         long long value = fib_value(yytext, yyleng);
         print_token(TAG_FIB, value, 1);
    }
}

{IDENT} {
    int valid = 1;
    if (yyleng > 0) {
         int prev_is_vowel = is_vowel(yytext[0]);
         for (int i = 1; i < yyleng; i++) {
              int curr_is_vowel = is_vowel(yytext[i]);
              if (curr_is_vowel == prev_is_vowel) { valid = 0; break; }
              prev_is_vowel = curr_is_vowel;
         }
    }
    if (!valid) {
         add_error("Error: Invalid identifier (letters must alternate vowels and consonants) at (%d,%d)-(%d,%d)", token_start_line, token_start_col, token_end_line, token_end_col);
    } else {
         int index = add_identifier(yytext);
         print_token(TAG_IDENT, index, 1);
    }
}

. {
    add_error("Error: Unexpected character '%s' at (%d,%d)-(%d,%d)", yytext, token_start_line, token_start_col, token_end_line, token_end_col);
}

%%

