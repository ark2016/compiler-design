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
    
    // Структура для отслеживания выделенной памяти
    typedef struct MemoryTracker {
        void** pointers;        // Массив указателей на выделенную память
        size_t count;           // Текущее количество указателей
        size_t capacity;        // Емкость массива
    } MemoryTracker;
}

%code provides {
    // Используем L2STYPE и L2LTYPE, так как они генерируются bison с нашим префиксом
    // Переименовываем параметры, чтобы соответствовать ожиданиям flex
    #define YY_DECL int l2lex(L2STYPE* yylval_param, L2LTYPE* yylloc_param, void* yyscanner)
    
    extern int l2lex_init(void** scanner);
    extern int l2lex_destroy(void* scanner);
    extern void l2set_in(FILE* in_str, void* yyscanner);
    
    void init_formatter(Formatter* formatter, int max_line_length);
    
    // Объявления функций для управления памятью
    void init_memory_tracker(MemoryTracker* tracker);
    void* track_memory(MemoryTracker* tracker, void* ptr);
    char* tracked_strdup(MemoryTracker* tracker, const char* str);
    void free_all_memory(MemoryTracker* tracker);
}

%code {
    int l2error(L2LTYPE* locp, void* scanner, Formatter* formatter, MemoryTracker* memory_tracker, const char* msg);
    
    void init_formatter(Formatter* formatter, int max_line_length) {
        formatter->indent_level = 0;
        formatter->indent_size = 2;
        formatter->max_line_length = max_line_length;
    }
    
    // Инициализация трекера памяти
    void init_memory_tracker(MemoryTracker* tracker) {
        const size_t initial_capacity = 100;
        tracker->pointers = (void**)malloc(initial_capacity * sizeof(void*));
        if (!tracker->pointers) {
            fprintf(stderr, "Ошибка выделения памяти для трекера\n");
            exit(EXIT_FAILURE);
        }
        tracker->count = 0;
        tracker->capacity = initial_capacity;
    }
    
    // Добавление указателя в трекер
    void* track_memory(MemoryTracker* tracker, void* ptr) {
        if (!ptr) return NULL;
        
        // Расширение массива при необходимости
        if (tracker->count >= tracker->capacity) {
            size_t new_capacity = tracker->capacity * 2;
            void** new_pointers = (void**)realloc(tracker->pointers, 
                                                  new_capacity * sizeof(void*));
            if (!new_pointers) {
                fprintf(stderr, "Ошибка расширения трекера памяти\n");
                exit(EXIT_FAILURE);
            }
            tracker->pointers = new_pointers;
            tracker->capacity = new_capacity;
        }
        
        // Добавление указателя
        tracker->pointers[tracker->count++] = ptr;
        return ptr;
    }
    
    // Создание копии строки и отслеживание её
    char* tracked_strdup(MemoryTracker* tracker, const char* str) {
        if (!str) return NULL;
        char* new_str = strdup(str);
        if (!new_str) {
            fprintf(stderr, "Ошибка выделения памяти для строки\n");
            exit(EXIT_FAILURE);
        }
        return (char*)track_memory(tracker, new_str);
    }
    
    // Освобождение всей отслеживаемой памяти
    void free_all_memory(MemoryTracker* tracker) {
        for (size_t i = 0; i < tracker->count; i++) {
            free(tracker->pointers[i]);
        }
        free(tracker->pointers);
        tracker->pointers = NULL;
        tracker->count = 0;
        tracker->capacity = 0;
    }
    
    // Вспомогательные функции
    void print_indented(int level, int size) {
        for (int i = 0; i < level * size; i++) {
            putchar(' ');
        }
    }
    
    // Добавляем функцию для создания строки с отступами
    char* create_indented_string(int level, int size, const char* str) {
        char* result = (char*)malloc(strlen(str) + level * size + 1);
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
%parse-param {MemoryTracker* memory_tracker}
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
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

func_header     : type_or_void IDENTIFIER {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | type_or_void IDENTIFIER CALL_OP formal_params {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s %s", $1, $2, $3, $4);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

formal_params   : param {
                    $$ = $1;
                  }
                | formal_params COMMA param {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

param           : type IDENTIFIER {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    char* stmt_list = tracked_strdup(memory_tracker, $1);
                    char* line = strtok(stmt_list, "\n");
                    
                    // Применяем отступы к каждой строке блока
                    while (line != NULL) {
                        char* indented = create_indented_string(1, formatter->indent_size, line);
                        if (indented) {
                            track_memory(memory_tracker, indented);
                            if (strlen(buffer) > 0) {
                                strcat(buffer, "\n");
                            }
                            strcat(buffer, indented);
                            // Не освобождаем indented здесь, это будет сделано с помощью memory_tracker
                        }
                        line = strtok(NULL, "\n");
                    }
                    
                    // Не освобождаем stmt_list, это будет сделано с помощью memory_tracker
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | decl_stmt COMMA decl_item {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

decl_item       : IDENTIFIER {
                    $$ = $1;
                  }
                | IDENTIFIER ASSIGN expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

assign_stmt     : lvalue ASSIGN expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

lvalue          : IDENTIFIER {
                    $$ = $1;
                  }
                | lvalue LBRACKET expr RBRACKET {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s%s%s", $1, $2, $3, $4);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

call_stmt       : IDENTIFIER CALL_OP arg_list {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

arg_list        : /* пусто */ {
                    $$ = tracked_strdup(memory_tracker, "");
                  }
                | expr {
                    $$ = $1;
                  }
                | arg_list COMMA expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s%s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

if_stmt         : expr THEN block DOT {
                    char buffer[1024];
                    // Форматируем if-then блок с правильными отступами
                    formatter->indent_level++;
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $4);
                    if (dot_indented) {
                        track_memory(memory_tracker, dot_indented);
                    }
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s\n%s", $1, $2, $3, dot_indented ? dot_indented : "");
                    // Не освобождаем dot_indented, это будет сделано memory_tracker
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | expr THEN block ELSE block DOT {
                    char buffer[1024];
                    // Форматируем if-then-else блок с правильными отступами
                    formatter->indent_level++;
                    char* else_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $4);
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $6);
                    if (else_indented) {
                        track_memory(memory_tracker, else_indented);
                    }
                    if (dot_indented) {
                        track_memory(memory_tracker, dot_indented);
                    }
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s\n%s\n%s\n%s", 
                             $1, $2, $3, else_indented ? else_indented : "", 
                             $5, dot_indented ? dot_indented : "");
                    // Не освобождаем else_indented и dot_indented, это будет сделано memory_tracker
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

while_stmt      : expr LOOP block DOT {
                    char buffer[1024];
                    // Форматируем while-loop блок с правильными отступами
                    formatter->indent_level++;
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $4);
                    if (dot_indented) {
                        track_memory(memory_tracker, dot_indented);
                    }
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s\n%s\n%s", $1, $2, $3, dot_indented ? dot_indented : "");
                    // Не освобождаем dot_indented, это будет сделано memory_tracker
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

for_stmt        : expr TILDE expr LOOP IDENTIFIER block DOT {
                    char buffer[1024];
                    // Форматируем for блок с правильными отступами
                    formatter->indent_level++;
                    char* dot_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $7);
                    if (dot_indented) {
                        track_memory(memory_tracker, dot_indented);
                    }
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s %s %s %s %s\n%s\n%s", 
                             $1, $2, $3, $4, $5, $6, dot_indented ? dot_indented : "");
                    // Не освобождаем dot_indented, это будет сделано memory_tracker
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

do_while_stmt   : LOOP block WHILE expr DOT {
                    char buffer[1024];
                    // Форматируем do-while блок с правильными отступами
                    formatter->indent_level++;
                    char* while_indented = create_indented_string(formatter->indent_level, formatter->indent_size, $3);
                    if (while_indented) {
                        track_memory(memory_tracker, while_indented);
                    }
                    formatter->indent_level--;
                    snprintf(buffer, sizeof(buffer), "%s\n%s\n%s %s %s", $1, $2, 
                            while_indented ? while_indented : "", $4, $5);
                    // Не освобождаем while_indented, это будет сделано memory_tracker
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

return_stmt     : RETURN {
                    // Применяем текущий уровень отступов
                    $$ = $1;
                  }
                | RETURN expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

logic_xor       : logic_and {
                    $$ = $1;
                  }
                | logic_xor XOR logic_and {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

logic_and       : equality {
                    $$ = $1;
                  }
                | logic_and AND equality {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

equality        : relation {
                    $$ = $1;
                  }
                | equality EQ relation {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | equality NEQ relation {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

relation        : call {
                    $$ = $1;
                  }
                | relation LT call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | relation GT call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | relation LEQ call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | relation GEQ call {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

call            : add_sub {
                    $$ = $1;
                  }
                | call CALL_OP arg_list {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

add_sub         : mul {
                    $$ = $1;
                  }
                | add_sub PLUS mul {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | add_sub MINUS mul {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

mul             : power {
                    $$ = $1;
                  }
                | mul MUL power {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | mul DIV power {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | mul MOD power {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

power           : unary {
                    $$ = $1;
                  }
                | power POW unary {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s %s", $1, $2, $3);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

unary           : primary {
                    $$ = $1;
                  }
                | MINUS unary {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                | NOT unary {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    $$ = tracked_strdup(memory_tracker, buffer);
                  }
                ;

alloc           : type expr {
                    char buffer[1024];
                    snprintf(buffer, sizeof(buffer), "%s %s", $1, $2);
                    $$ = tracked_strdup(memory_tracker, buffer);
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
                    $$ = tracked_strdup(memory_tracker, buffer);
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

int l2error(L2LTYPE* locp, void* scanner, Formatter* formatter, MemoryTracker* memory_tracker, const char* msg) {
    (void)scanner;        // Подавляем предупреждение о неиспользуемом параметре
    (void)formatter;      // Подавляем предупреждение о неиспользуемом параметре
    (void)memory_tracker; // Подавляем предупреждение о неиспользуемом параметре
    fprintf(stderr, "Ошибка синтаксиса в строке %d, позиция %d: %s\n", locp->first_line, locp->first_column, msg);
    return 0;
} 