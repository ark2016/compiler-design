#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <getopt.h>

// Удаляем определение YYLTYPE, которое конфликтует с L2LTYPE
// и включаем заголовок сразу
#include "l2.tab.h"

extern int l2parse(void* scanner, Formatter* formatter);

void print_usage(const char* program_name) {
    printf("Использование: %s [ОПЦИИ] [ФАЙЛ]\n", program_name);
    printf("Опции:\n");
    printf("  -w, --width=ШИРИНА   Максимальная ширина строки (по умолчанию 80)\n");
    printf("  -h, --help           Показать справку и выйти\n");
}

int main(int argc, char* argv[]) {
    int max_line_length = 80; // Значение по умолчанию
    char* input_file = NULL;
    
    // Опции командной строки
    static struct option long_options[] = {
        {"width", required_argument, 0, 'w'},
        {"help", no_argument, 0, 'h'},
        {0, 0, 0, 0}
    };
    
    int option_index = 0;
    int c;
    
    while ((c = getopt_long(argc, argv, "w:h", long_options, &option_index)) != -1) {
        switch (c) {
            case 'w':
                max_line_length = atoi(optarg);
                if (max_line_length <= 0) {
                    fprintf(stderr, "Ширина строки должна быть положительным числом\n");
                    return 1;
                }
                break;
            case 'h':
                print_usage(argv[0]);
                return 0;
            case '?':
                return 1;
            default:
                abort();
        }
    }
    
    // Если после опций остались аргументы, первый - имя входного файла
    if (optind < argc) {
        input_file = argv[optind];
    }
    
    // Открываем входной файл или используем stdin
    FILE* in_file = stdin;
    if (input_file) {
        in_file = fopen(input_file, "r");
        if (!in_file) {
            fprintf(stderr, "Не удалось открыть файл %s\n", input_file);
            return 1;
        }
    }
    
    // Инициализация сканера
    void* scanner;
    l2lex_init(&scanner);
    l2set_in(in_file, scanner);
    
    // Инициализация форматтера
    Formatter formatter;
    init_formatter(&formatter, max_line_length);
    
    // Запуск анализатора
    int result = l2parse(scanner, &formatter);
    
    // Освобождение ресурсов
    l2lex_destroy(scanner);
    
    if (in_file != stdin) {
        fclose(in_file);
    }
    
    return result;
} 