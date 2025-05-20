%define api.pure full
%define parse.error verbose
%locations
%define api.prefix {l2}

%code requires {
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <stdbool.h>

    typedef struct {
        char* string;
    } l2_token;

    typedef struct Formatter {
        int indent_level;       // Текущий уровень отступа
        int indent_size;        // Размер отступа (в пробелах)
        int max_line_length;    // Максимальная длина строки
        int current_line_length; // Текущая длина строки
        bool need_indent;       // Нужно ли добавить отступ перед следующим токеном
        bool need_space;        // Нужно ли добавить пробел перед следующим токеном
        bool in_expr;           // Находимся ли внутри выражения
        int expr_level;         // Уровень вложенности выражений
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
    void print_token(Formatter* formatter, const char* token);
    void print_newline(Formatter* formatter);
    void increase_indent(Formatter* formatter);
    void decrease_indent(Formatter* formatter);
}

%code {
    int l2error(L2LTYPE* locp, void* scanner, Formatter* formatter, const char* msg);
    
    void init_formatter(Formatter* formatter, int max_line_length) {
        formatter->indent_level = 0;
        formatter->indent_size = 2;
        formatter->max_line_length = max_line_length;
        formatter->current_line_length = 0;
        formatter->need_indent = true;
        formatter->need_space = false;
        formatter->in_expr = false;
        formatter->expr_level = 0;
    }
    
    void print_indent(Formatter* formatter) {
        for (int i = 0; i < formatter->indent_level * formatter->indent_size; i++) {
            putchar(' ');
            formatter->current_line_length++;
        }
    }
    
    void print_newline(Formatter* formatter) {
        putchar('\n');
        formatter->current_line_length = 0;
        formatter->need_indent = true;
        formatter->need_space = false;
    }
    
    void print_token(Formatter* formatter, const char* token) {
        int token_length = strlen(token);
        
        // Проверяем, нужен ли отступ
        if (formatter->need_indent) {
            print_indent(formatter);
            formatter->need_indent = false;
        } 
        // Проверяем, нужен ли пробел
        else if (formatter->need_space) {
            // Проверяем, не выйдет ли строка за предел
            if (formatter->current_line_length + 1 + token_length > formatter->max_line_length) {
                print_newline(formatter);
                print_indent(formatter);
            } else {
                putchar(' ');
                formatter->current_line_length++;
            }
            formatter->need_space = false;
        }
        
        // Выводим токен
        printf("%s", token);
        formatter->current_line_length += token_length;
        
        // После большинства токенов нужен пробел
        formatter->need_space = true;
    }
    
    void increase_indent(Formatter* formatter) {
        formatter->indent_level++;
    }
    
    void decrease_indent(Formatter* formatter) {
        if (formatter->indent_level > 0) {
            formatter->indent_level--;
        }
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

program         : func_def                   { print_newline(formatter); }
                | program func_def           { print_newline(formatter); }
                ;

func_def        : func_header EQUAL block DOT {
                    print_token(formatter, $4);
                  }
                ;

func_header     : type_or_void IDENTIFIER          {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                  }
                | type_or_void IDENTIFIER CALL_OP formal_params {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                    print_token(formatter, $4);
                  }
                ;

formal_params   : param                      { $$ = $1; }
                | formal_params COMMA param  { 
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

param           : type IDENTIFIER            { 
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                  }
                ;

type_or_void    : type                       { $$ = $1; }
                | VOID                       { $$ = $1; print_token(formatter, $1); }
                ;

type            : prim_type                  { $$ = $1; print_token(formatter, $1); }
                | type LBRACKET RBRACKET     { 
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

prim_type       : INT                        { $$ = $1; }
                | CHAR                       { $$ = $1; }
                | BOOL                       { $$ = $1; }
                ;

block           : stmt_list                  { 
                    print_newline(formatter);
                    print_newline(formatter);
                  }
                ;

stmt_list       : statement                  { $$ = $1; }
                | stmt_list SEMICOLON statement {
                    print_token(formatter, $2);
                    print_newline(formatter);
                    print_token(formatter, $3);
                  }
                ;

statement       : decl_stmt                  { $$ = $1; }
                | assign_stmt                { $$ = $1; }
                | call_stmt                  { $$ = $1; }
                | if_stmt                    { $$ = $1; }
                | while_stmt                 { $$ = $1; }
                | for_stmt                   { $$ = $1; }
                | do_while_stmt              { $$ = $1; }
                | return_stmt                { $$ = $1; }
                ;

decl_stmt       : type decl_item             { $$ = $1; }
                | decl_stmt COMMA decl_item  {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

decl_item       : IDENTIFIER                 { $$ = $1; print_token(formatter, $1); }
                | IDENTIFIER ASSIGN expr     {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

assign_stmt     : lvalue ASSIGN expr         {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

lvalue          : IDENTIFIER                 { $$ = $1; print_token(formatter, $1); }
                | lvalue LBRACKET expr RBRACKET {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                    print_token(formatter, $4);
                  }
                ;

call_stmt       : IDENTIFIER CALL_OP arg_list {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

arg_list        : /* пусто */                { $$ = ""; }
                | expr                       { $$ = $1; }
                | arg_list COMMA expr        {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

if_stmt         : expr THEN block DOT        {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    increase_indent(formatter);
                    print_token(formatter, $3);
                    decrease_indent(formatter);
                    print_token(formatter, $4);
                  }
                | expr THEN block ELSE block DOT {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    increase_indent(formatter);
                    print_token(formatter, $3);
                    decrease_indent(formatter);
                    print_token(formatter, $4);
                    increase_indent(formatter);
                    print_token(formatter, $5);
                    decrease_indent(formatter);
                    print_token(formatter, $6);
                  }
                ;

while_stmt      : expr LOOP block DOT        {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    increase_indent(formatter);
                    print_token(formatter, $3);
                    decrease_indent(formatter);
                    print_token(formatter, $4);
                  }
                ;

for_stmt        : expr TILDE expr LOOP IDENTIFIER block DOT {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                    print_token(formatter, $4);
                    print_token(formatter, $5);
                    increase_indent(formatter);
                    print_token(formatter, $6);
                    decrease_indent(formatter);
                    print_token(formatter, $7);
                  }
                ;

do_while_stmt   : LOOP block WHILE expr DOT  {
                    print_token(formatter, $1);
                    increase_indent(formatter);
                    print_token(formatter, $2);
                    decrease_indent(formatter);
                    print_token(formatter, $3);
                    print_token(formatter, $4);
                    print_token(formatter, $5);
                  }
                ;

return_stmt     : RETURN                     { $$ = $1; print_token(formatter, $1); }
                | RETURN expr                {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                  }
                ;

expr            : logic_or                   { 
                    $$ = $1; 
                    formatter->in_expr = true;
                    formatter->expr_level = 11;
                  }
                ;

logic_or        : logic_xor                  { $$ = $1; }
                | logic_or OR logic_xor      {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

logic_xor       : logic_and                  { $$ = $1; }
                | logic_xor XOR logic_and    {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

logic_and       : equality                   { $$ = $1; }
                | logic_and AND equality     {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

equality        : relation                   { $$ = $1; }
                | equality EQ relation       {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | equality NEQ relation      {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

relation        : call                       { $$ = $1; }
                | relation LT call           {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | relation GT call           {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | relation LEQ call          {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | relation GEQ call          {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

call            : add_sub                    { $$ = $1; }
                | call CALL_OP arg_list      {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

add_sub         : mul                        { $$ = $1; }
                | add_sub PLUS mul           {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | add_sub MINUS mul          {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

mul             : power                      { $$ = $1; }
                | mul MUL power              {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | mul DIV power              {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                | mul MOD power              {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

power           : unary                      { $$ = $1; }
                | power POW unary            {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

unary           : primary                    { $$ = $1; }
                | MINUS unary                {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                  }
                | NOT unary                  {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                  }
                ;

primary         : alloc                      { $$ = $1; }
                | atom                       { $$ = $1; }
                | atom LBRACKET expr RBRACKET {
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                    print_token(formatter, $4);
                  }
                ;

alloc           : type expr                  {
                    print_token(formatter, $2);
                  }
                ;

atom            : const                      { $$ = $1; }
                | IDENTIFIER                 { $$ = $1; print_token(formatter, $1); }
                | LPAREN expr RPAREN         {
                    print_token(formatter, $1);
                    print_token(formatter, $2);
                    print_token(formatter, $3);
                  }
                ;

const           : INT_CONST                  { $$ = $1; print_token(formatter, $1); }
                | CHAR_CONST                 { $$ = $1; print_token(formatter, $1); }
                | STRING_CONST               { $$ = $1; print_token(formatter, $1); }
                | TRUE_LIT                   { $$ = $1; print_token(formatter, $1); }
                | FALSE_LIT                  { $$ = $1; print_token(formatter, $1); }
                | NULL_LIT                   { $$ = $1; print_token(formatter, $1); }
                ;

%%

int l2error(L2LTYPE* locp, void* scanner, Formatter* formatter, const char* msg) {
    (void)scanner;     // Подавляем предупреждение о неиспользуемом параметре
    (void)formatter;   // Подавляем предупреждение о неиспользуемом параметре
    fprintf(stderr, "Ошибка синтаксиса в строке %d, позиция %d: %s\n", locp->first_line, locp->first_column, msg);
    return 0;
} 