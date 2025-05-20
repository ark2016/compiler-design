%define api.pure full
%define parse.error verbose
%locations
%define api.prefix {l2}

%code requires {
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <stdbool.h>

    typedef struct Formatter {
        int indent_level;       // Текущий уровень отступа
        int indent_size;        // Размер отступа (в пробелах)
        int max_line_length;    // Максимальная длина строки
    } Formatter;
}

%code provides {
    // Используем L2STYPE и L2LTYPE, так как они генерируются bison с нашим префиксом
    // Переименовываем параметры, чтобы соответствовать ожиданиям flex
    #define YY_DECL int l2lex(L2STYPE* yylval_param, L2LTYPE* yylloc_param, void* yyscanner)
    
    extern int l2lex_init(void** scanner);
    extern int l2lex_destroy(void* scanner);
    extern void l2set_in(FILE* in_str, void* yyscanner);
    
    void init_formatter(Formatter* formatter, int max_line_length);
}

%code {
    int l2error(L2LTYPE* locp, void* scanner, Formatter* formatter, const char* msg);
    
    void init_formatter(Formatter* formatter, int max_line_length) {
        formatter->indent_level = 0;
        formatter->indent_size = 2;
        formatter->max_line_length = max_line_length;
    }
    
    // Вспомогательные функции
    void print_indented(int level, int size) {
        for (int i = 0; i < level * size; i++) {
            putchar(' ');
        }
    }
    
    // Добавляем функцию для создания строки с отступами
    char* create_indented_string(int level, int size, const char* str) {
        char* result = malloc(strlen(str) + level * size + 1);
        if (!result) return NULL;
        
        int i;
        for (i = 0; i < level * size; i++) {
            result[i] = ' ';
        }
        strcpy(result + i, str);
        return result;
    }
}

%parse-param {void* scanner}
%parse-param {Formatter* formatter}
%lex-param {void* scanner}

%union {
    char* string;
}

/* Объявление токенов */
%token <string> IDENTIFIER INT_CONST CHAR_CONST STRING_CONST
%token <string> BOOL CHAR INT VOID RETURN LOOP WHILE THEN ELSE
%token <string> NULL_LIT TRUE_LIT FALSE_LIT COMMENT
%token <string> ASSIGN CALL_OP EQ NEQ LEQ GEQ
%token <string> AND OR XOR NOT PLUS MINUS MUL DIV MOD POW EQUAL
%token <string> SEMICOLON COMMA DOT LBRACKET RBRACKET LPAREN RPAREN
%token <string> LT GT TILDE
%token <string> UNKNOWN

%type <string> program func_def func_header formal_params param
%type <string> type_or_void type prim_type block stmt_list statement
%type <string> decl_stmt decl_item assign_stmt lvalue
%type <string> call_stmt arg_list if_stmt while_stmt for_stmt do_while_stmt return_stmt
%type <string> expr logic_or logic_xor logic_and equality relation
%type <string> call add_sub mul power unary primary alloc atom const

%start program

%%

program         : func_def                   { printf("%s\n", $1); }
                | program func_def           { printf("\n%s\n", $2); }
                ;

func_def        : func_header EQUAL block DOT {
                    char buffer[1024];
                    // Выводим заголовок функции, знак равенства, затем блок с отступом и точку
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s%s", $1, $2, $3, $4);
                    $$ = strdup(buffer);
                  }
                ;

func_header     : type_or_void IDENTIFIER {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                | type_or_void IDENTIFIER CALL_OP formal_params {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s %s", $1, $2, $3, $4);
                    $$ = strdup(buffer);
                  }
                ;

formal_params   : param {
                    $$ = $1;
                  }
                | formal_params COMMA param {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

param           : type IDENTIFIER {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                ;

type_or_void    : type {
                    $$ = $1;
                  }
                | VOID {
                    $$ = $1;
                  }
                ;

type            : prim_type {
                    $$ = $1;
                  }
                | type LBRACKET RBRACKET {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s%s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

prim_type       : INT {
                    $$ = $1;
                  }
                | CHAR {
                    $$ = $1;
                  }
                | BOOL {
                    $$ = $1;
                  }
                ;

block           : stmt_list {
                    char buffer[4096] = "";
                    char* stmt_list = strdup($1);
                    char* line = strtok(stmt_list, "\n");
                    
                    // Применяем отступы к каждой строке блока
                    while (line != NULL) {
                        char* indented = create_indented_string(1, formatter->indent_size, line);
                        if (strlen(buffer) > 0) {
                            strcat(buffer, "\n");
                        }
                        strcat(buffer, indented);
                        free(indented);
                        line = strtok(NULL, "\n");
                    }
                    
                    free(stmt_list);
                    $$ = strdup(buffer);
                  }
                ;

stmt_list       : statement {
                    $$ = $1;
                  }
                | stmt_list SEMICOLON statement {
                    char buffer[2048];
                    // Добавляем точку с запятой к предыдущему оператору
                    // Затем делаем перенос строки и сохраняем новый оператор
                    // Отступы будут применены к каждому оператору в блоке на уровне block
                    snprintf(buffer, sizeof(buffer), "%s%s\n%s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

statement       : decl_stmt { $$ = $1; }
                | assign_stmt { $$ = $1; }
                | call_stmt { $$ = $1; }
                | if_stmt { $$ = $1; }
                | while_stmt { $$ = $1; }
                | for_stmt { $$ = $1; }
                | do_while_stmt { $$ = $1; }
                | return_stmt { $$ = $1; }
                ;

decl_stmt       : type decl_item {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                | decl_stmt COMMA decl_item {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

decl_item       : IDENTIFIER {
                    $$ = $1;
                  }
                | IDENTIFIER ASSIGN expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

assign_stmt     : lvalue ASSIGN expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

lvalue          : IDENTIFIER {
                    $$ = $1;
                  }
                | lvalue LBRACKET expr RBRACKET {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s%s%s", $1, $2, $3, $4);
                    $$ = strdup(buffer);
                  }
                ;

call_stmt       : IDENTIFIER CALL_OP arg_list {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

arg_list        : /* пусто */ {
                    $$ = strdup("");
                  }
                | expr {
                    $$ = $1;
                  }
                | arg_list COMMA expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

if_stmt         : expr THEN block DOT {
                    char buffer[1024];
                    // Форматируем if-then блок с правильными отступами
                    formatter->indent_level++;
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $4);
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s\n%s", $1, $2, $3, dot_indented);
                    free(dot_indented);
                    $$ = strdup(buffer);
                  }
                | expr THEN block ELSE block DOT {
                    char buffer[1024];
                    // Форматируем if-then-else блок с правильными отступами
                    formatter->indent_level++;
                    char* else_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $4);
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $6);
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s\n%s\n%s\n%s", 
                             $1, $2, $3, else_indented, $5, dot_indented);
                    free(else_indented);
                    free(dot_indented);
                    $$ = strdup(buffer);
                  }
                ;

while_stmt      : expr LOOP block DOT {
                    char buffer[1024];
                    // Форматируем while-loop блок с правильными отступами
                    formatter->indent_level++;
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $4);
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s\n%s", $1, $2, $3, dot_indented);
                    free(dot_indented);
                    $$ = strdup(buffer);
                  }
                ;

for_stmt        : expr TILDE expr LOOP IDENTIFIER block DOT {
                    char buffer[1024];
                    // Форматируем for блок с правильными отступами
                    formatter->indent_level++;
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $7);
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s %s %s %s\n%s\n%s", 
                             $1, $2, $3, $4, $5, $6, dot_indented);
                    free(dot_indented);
                    $$ = strdup(buffer);
                  }
                ;

do_while_stmt   : LOOP block WHILE expr DOT {
                    char buffer[1024];
                    // Форматируем do-while блок с правильными отступами
                    formatter->indent_level++;
                    char* while_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $3);
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s\n%s\n%s %s %s", $1, $2, while_indented, $4, $5);
                    free(while_indented);
                    $$ = strdup(buffer);
                  }
                ;

return_stmt     : RETURN {
                    // Применяем текущий уровень отступов
                    $$ = $1;
                  }
                | RETURN expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                ;

expr            : logic_or {
                    $$ = $1;
                  }
                ;

logic_or        : logic_xor {
                    $$ = $1;
                  }
                | logic_or OR logic_xor {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

logic_xor       : logic_and {
                    $$ = $1;
                  }
                | logic_xor XOR logic_and {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

logic_and       : equality {
                    $$ = $1;
                  }
                | logic_and AND equality {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

equality        : relation {
                    $$ = $1;
                  }
                | equality EQ relation {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | equality NEQ relation {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

relation        : call {
                    $$ = $1;
                  }
                | relation LT call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | relation GT call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | relation LEQ call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | relation GEQ call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

call            : add_sub {
                    $$ = $1;
                  }
                | call CALL_OP arg_list {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

add_sub         : mul {
                    $$ = $1;
                  }
                | add_sub PLUS mul {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | add_sub MINUS mul {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

mul             : power {
                    $$ = $1;
                  }
                | mul MUL power {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | mul DIV power {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                | mul MOD power {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

power           : unary {
                    $$ = $1;
                  }
                | power POW unary {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

unary           : primary {
                    $$ = $1;
                  }
                | MINUS unary {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                | NOT unary {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                ;

primary         : alloc {
                    $$ = $1;
                  }
                | atom {
                    $$ = $1;
                  }
                | atom LBRACKET expr RBRACKET {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s%s%s", $1, $2, $3, $4);
                    $$ = strdup(buffer);
                  }
                ;

alloc           : type expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = strdup(buffer);
                  }
                ;

atom            : const {
                    $$ = $1;
                  }
                | IDENTIFIER {
                    $$ = $1;
                  }
                | LPAREN expr RPAREN {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s%s", $1, $2, $3);
                    $$ = strdup(buffer);
                  }
                ;

const           : INT_CONST {
                    $$ = $1;
                  }
                | CHAR_CONST {
                    $$ = $1;
                  }
                | STRING_CONST {
                    $$ = $1;
                  }
                | TRUE_LIT {
                    $$ = $1;
                  }
                | FALSE_LIT {
                    $$ = $1;
                  }
                | NULL_LIT {
                    $$ = $1;
                  }
                ;

%%

int l2error(L2LTYPE* locp, void* scanner, Formatter* formatter, const char* msg) {
    (void)scanner;     // Подавляем предупреждение о неиспользуемом параметре
    (void)formatter;   // Подавляем предупреждение о неиспользуемом параметре
    fprintf(stderr, "Ошибка синтаксиса в строке %d, позиция %d: %s\n", locp->first_line, locp->first_column, msg);
    return 0;
} 