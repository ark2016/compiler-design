#include <stdio.h>
#include <stdlib.h>

/* Функции, генерируемые flex */
extern int yylex(void);
extern void yy_scan_string(const char*);
extern void report_errors(void);

int main(int argc, char **argv) {
    if (argc < 2) {
         fprintf(stderr, "Usage: %s input.txt\n", argv[0]);
         return 1;
    }
    /* Открываем входной файл */
    FILE *f = fopen(argv[1], "r");
    if (!f) {
         perror("fopen");
         return 1;
    }
    /* Определяем размер файла и считываем содержимое */
    fseek(f, 0, SEEK_END);
    long fsize = ftell(f);
    fseek(f, 0, SEEK_SET);
    char *input = malloc(fsize + 1);
    if (!input) {
         perror("malloc");
         return 1;
    }
    fread(input, 1, fsize, f);
    fclose(f);
    input[fsize] = '\0';

    /* Передаем строку в лексический анализатор */
    yy_scan_string(input);
    free(input);

    /* Запускаем лексический анализ */
    yylex();

    /* Выводим собранные ошибки */
    report_errors();

    return 0;
}

