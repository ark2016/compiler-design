import re


class Token:
    def __init__(self, tag, line, col, value):
        self.tag = tag  # например KEYWORD, VARIABLE, INTEGER и т.д.
        self.line = line
        self.col = col
        self.value = value

    def __str__(self):
        return f"{self.tag} ({self.line}, {self.col}): {self.value}"


class Lexer:
    """
    Лексический анализатор, работающий как итератор токенов.
    Имеет метод nextToken(), возвращающий следующий токен или None.
    По условию, при ошибке пропускает все подряд идущие символы до ближайшей корректной лексемы.
    """

    def __init__(self, file_path):
        with open(file_path, "r", encoding="utf-8") as f:
            self.lines = f.readlines()

        self.current_line = 0
        self.current_text = ""
        self.line_length = 0
        self.col = 0

        # Регулярные выражения для каждой категории токенов порядок в списке определяет приоритет.
        self.TOKEN_PATTERNS = [
            # 1. Ключевые слова: sub, if, unless (полное слово => \b), (?: ... ) не создаёт группы для извлечения, а
            # просто группирует выражение, ускоряя обработку.
            ("KEYWORD", re.compile(r"^(?:sub|if|unless)\b", re.UNICODE)),

            # 2. Имена функций: начинаются на Unicode-букву, далее буквы или цифры
            #   [^\W\d_] — это любая "буква" (\w) без цифр \d и подчёркивания _
            #   [^\W_]*  — любая комбинация букв и цифр (\w без "_")
            ("FUNCTION", re.compile(r"^[^\W\d_][^\W_]*", re.UNICODE)),

            # 3. Идентификаторы переменных: начинаются на $, @ или %, далее буквы/цифры
            ("VARIABLE", re.compile(r"^[\$@%][^\W_]+", re.UNICODE)),

            # 4. Вещественные числа (десятичные цифры + опционально дробная часть + опционально порядок)
            #    Дробная часть: ,<цифры>
            #    Порядок: [eE][+\-]?<цифры>
            #    По условию может отсутствовать дробная часть или порядок, но не оба сразу (иначе INTEGER)
            ("REAL", re.compile(r"^[0-9]+(?:,[0-9]+(?:[eE][+\-]?[0-9]+)?|[eE][+\-]?[0-9]+)", re.UNICODE)),

            # 5. Целые числа: непустая последовательность цифр
            ("INTEGER", re.compile(r"^[0-9]+", re.UNICODE)),
        ]

        # Изначально загружаем первую строку, если она есть
        self._load_line()

    def _load_line(self):
        if self.current_line < len(self.lines):
            self.current_text = self.lines[self.current_line]
            self.line_length = len(self.current_text)
            self.col = 0
        else:
            self.current_text = ""
            self.line_length = 0
            self.col = 0

    def nextToken(self):
        """
        Возвращает очередной распознанный токен или None, если достигнут конец файла.
        При ошибке выводит одно сообщение вида "syntax error (line,col)" и
        пропускает весь "плохой" участок до следующей корректной лексемы.
        """
        while True:
            if self.current_line >= len(self.lines):
                return None

            if self.col >= self.line_length:
                self.current_line += 1
                if self.current_line >= len(self.lines):
                    return None
                self._load_line()
                continue

            # пробелы/табуляции и т.д.
            while self.col < self.line_length and self.current_text[self.col].isspace():
                self.col += 1
            if self.col >= self.line_length:
                continue  # строка закончилась, переходим к следующей итерации

            # Смотрим шаблоны
            substring = self.current_text[self.col:]
            for token_type, pattern in self.TOKEN_PATTERNS:
                m = pattern.match(substring)
                if m:
                    # Нашли лексему
                    value = m.group(0)
                    line_num = self.current_line + 1
                    col_num = self.col + 1
                    self.col += len(value)
                    return Token(token_type, line_num, col_num, value)

            # никакой шаблон не подошёл (ошибка)
            error_line = self.current_line + 1
            error_col = self.col + 1
            print(f"syntax error ({error_line},{error_col})")

            # По условию: пропускаем все подряд идущие символы, пока не найдём лексему или не кончится строка. Чтобы
            # выдать 1 сообщение об ошибке за блок "плохих" символов, печатаем ошибку один раз и потом пропускаем всё
            # подряд, пока не найдём совпадение.
            self._skip_bad_chunk()
            # После пропуска возвращаемся в основной цикл и пробуем match заново

    def _skip_bad_chunk(self):
        """
        Пропускает все символы до тех пор, пока не будет найден какой-то токен (или пока не закончится текущая строка).
        То есть восстанавливается до первой найденной лексемы или конца строки.
        """
        while self.col < self.line_length:
            substring = self.current_text[self.col:]
            # Проверяем, не начинается ли здесь валидный токен
            if any(pattern.match(substring) for _, pattern in self.TOKEN_PATTERNS):
                # Если нашли совпадение, прекращаем пропуск
                return
            self.col += 1


def main():
    # Тестовый файл, который вы можете создать рядом
    file_path = "test_input.txt"

    lexer = Lexer(file_path)
    token = lexer.nextToken()
    while token:
        # token = lexer.nextToken()
        # if token is None: # конец файла
        #     break
        print(token)
        token = lexer.nextToken()


if __name__ == "__main__":
    main()
