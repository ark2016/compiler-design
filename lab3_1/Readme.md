
# Лабораторная работа № 3.1: Самоприменимый генератор компиляторов на основе предсказывающего анализа

##  Структура проекта

Проект состоит из нескольких модулей:

*   [`common.py`](common.py): Общие определения (токены, узлы AST, классы исключений) и утилиты (генерация DOT-представления дерева).
*   [`gdl_lexer.py`](gdl_lexer.py): Лексический анализатор для языка описания грамматик (GDL).
*   [`gdl_parser_hardcoded.py`](gdl_parser_hardcoded.py): Предсказывающий синтаксический анализатор для GDL с жестко закодированной логикой. Используется для разбора GDL-файлов на **Этапе 1** и **Шаге 3** раскрутки (для генерации первой таблицы GDL).
*   [`grammar_analyzer.py`](grammar_analyzer.py): Модуль, содержащий логику анализа грамматики (извлечение из AST, FIRST, FOLLOW, построение LL(1) таблицы).
*   [`compiler_generator.py`](compiler_generator.py): Основной скрипт **Этапа 1**. Использует `gdl_lexer`, `gdl_parser_hardcoded`, `grammar_analyzer`.
*   [`generic_ll1_parser.py`](generic_ll1_parser.py): Универсальный предсказывающий синтаксический анализатор, работающий на основе переданной ему LL(1) таблицы разбора. Используется в **Калькуляторе** (Этап 2) и **Bootstrapper'е** (Этап 4).
*   [`calculator_lexer.py`](calculator_lexer.py): Лексический анализатор для языка калькулятора.
*   [`calculator_evaluator.py`](calculator_evaluator.py): (Опционально) Модуль для вычисления значения выражения по его AST.
*   [`calculator.py`](calculator.py): Основной скрипт **Этапа 2** (тестовая программа "Калькулятор"). Использует `calculator_lexer`, `generic_ll1_parser` и `calculator_evaluator`.
*   [`bootstrapper.py`](bootstrapper.py): Скрипт для демонстрации **Раскрутки** (Этап 4). Использует `gdl_lexer`, `generic_ll1_parser` (с таблицей для GDL) и `grammar_analyzer`.

**Файлы грамматик:**

*   [`calculator_grammar.gdl`](calculator_grammar.gdl): Описание грамматики простого калькулятора на языке GDL.
*   [`gdl_grammar_for_bootstrap.gdl`](gdl_grammar_for_bootstrap.gdl): Описание грамматики самого языка GDL на языке GDL.

**Тестовый ввод:**

*   [`expression.txt`](expression.txt): Файл с примером арифметического выражения.

## Язык описания грамматик (GDL)

Синтаксис GDL определен в `gdl_grammar_for_bootstrap.gdl` и реализован парсером `gdl_parser_hardcoded.py`. Кратко:
- Правила заключены в `[...]`.
- Аксиома: `[axiom [ИМЯ_АКСИОМЫ]]`.
- Определение правил для нетерминала: `[ИМЯ_НЕТЕРМИНАЛА [ПраваяЧасть1] [ПраваяЧасть2] ...]`.
- Правая часть правила (`[ПраваяЧастьN]`) – это последовательность символов: `[Символ1 Символ2 ...]`. Пустая правая часть `[]` обозначает ε.
- Символы: `IDENT` (нетерминалы или терминалы), `OP`, `LPAREN`, `RPAREN`, `KW_N` ('n'), `EOF`.
- Комментарии: `%` до конца строки.

## Запуск и тестирование

Предполагается, что все файлы проекта находятся в одной директории и установлен Python 3.x. Для визуализации DOT-файлов требуется установленный Graphviz (`dot` в PATH).

**Шаг 1: Генерация таблицы для грамматики калькулятора (Используя жестко закодированный парсер GDL)**

```bash
# Создание таблицы разбора для грамматики калькулятора
python compiler_generator.py calculator_grammar.gdl -o calc_table.py --dot_gdl_ast calc_gdl_ast.dot --show_first_follow
```
*   Вывод: `calc_table.py` (таблица для калькулятора), `calc_gdl_ast.dot` (AST `calculator_grammar.gdl` построенное `gdl_parser_hardcoded.py`).

**Шаг 2: Тестирование калькулятора (Используя сгенерированную таблицу)**

```bash
# Разбор и вычисление выражения с помощью сгенерированной таблицы calc_table.py
python calculator.py calc_table.py expression.txt -o calc_expr_parsetree.dot --eval
```
*   Вывод: Результат вычисления в консоль, `calc_expr_parsetree.dot` (дерево разбора выражения).

**Шаг 3: Генерация таблицы для грамматики самого GDL (Подготовка к раскрутке)**

Эта команда использует `compiler_generator.py` (с жестко закодированным парсером) для обработки `gdl_grammar_for_bootstrap.gdl` и создания таблицы разбора для языка GDL (`gdl_table.py`).

```bash
# Создание таблицы разбора для грамматики GDL
python compiler_generator.py gdl_grammar_for_bootstrap.gdl -o gdl_table.py --dot_gdl_ast gdl_bootstrap_ast.dot --show_first_follow
```
*   Вывод: `gdl_table.py` (таблица для GDL), `gdl_bootstrap_ast.dot` (AST `gdl_grammar_for_bootstrap.gdl` построенное `gdl_parser_hardcoded.py`).

**Шаг 4: Раскрутка - Генерация таблицы калькулятора с помощью Bootstrapper**

Теперь `bootstrapper.py` (использующий `generic_ll1_parser` и таблицу `gdl_table.py`) парсит `calculator_grammar.gdl` и генерирует таблицу разбора для калькулятора.

```bash
# Генерация таблицы калькулятора с использованием bootstrapper'а и таблицы GDL
python bootstrapper.py gdl_table.py calculator_grammar.gdl -o calc_table_bootstrapped.py --dot_user_gdl_ast calc_gdl_ast_boot.dot
```
*   Вывод: `calc_table_bootstrapped.py` (таблица для калькулятора, сгенерированная `bootstrapper.py`), `calc_gdl_ast_boot.dot` (AST `calculator_grammar.gdl` построенное `generic_ll1_parser` по таблице GDL). Сравните `calc_table_bootstrapped.py` с `calc_table.py` – они должны быть идентичны.

**Шаг 5 (Опционально): Полная самоприменимость**

Демонстрация генерации таблицы GDL с помощью `bootstrapper.py`, который сам использует таблицу GDL.

```bash
# Генерация таблицы GDL с использованием bootstrapper'а и таблицы GDL же
python bootstrapper.py gdl_table.py gdl_grammar_for_bootstrap.gdl -o gdl_table_fully_bootstrapped.py
```
*   Вывод: `gdl_table_fully_bootstrapped.py`. Сравните с `gdl_table.py` – они должны быть идентичны.

## Визуализация деревьев разбора (Graphviz)

После генерации `.dot` файлов, их можно преобразовать в изображения (например, PNG) с помощью утилиты `dot` из пакета Graphviz:

```bash
dot -Tpng filename.dot -o filename.png
```

## Зависимости

*   Python 3.x
*   Graphviz (для визуализации `.dot` файлов, опционально)

## Реализованные файлы

*   [`common.py`](common.py)
*   [`gdl_lexer.py`](gdl_lexer.py)
*   [`gdl_parser_hardcoded.py`](gdl_parser_hardcoded.py)
*   [`grammar_analyzer.py`](grammar_analyzer.py)
*   [`compiler_generator.py`](compiler_generator.py)
*   [`generic_ll1_parser.py`](generic_ll1_parser.py)
*   [`calculator_lexer.py`](calculator_lexer.py)
*   [`calculator_evaluator.py`](calculator_evaluator.py)
*   [`calculator.py`](calculator.py)
*   [`bootstrapper.py`](bootstrapper.py)
*   [`calculator_grammar.gdl`](calculator_grammar.gdl)
*   [`gdl_grammar_for_bootstrap.gdl`](gdl_grammar_for_bootstrap.gdl)
*   [`expression.txt`](expression.txt)

