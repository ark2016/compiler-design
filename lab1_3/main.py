#!/usr/bin/env python3
import sys


class Token:
    def __init__(self, type, lexeme, start_line, start_col, end_line, end_col, attribute=None):
        self.type = type
        self.lexeme = lexeme
        self.start_line = start_line
        self.start_col = start_col
        self.end_line = end_line
        self.end_col = end_col
        self.attribute = attribute

    def __str__(self):
        attr_str = f"{self.attribute}" if self.attribute is not None else ""
        return f"{self.type} ({self.start_line}, {self.start_col})-({self.end_line}, {self.end_col}): {attr_str}"


class Lexer:
    global_id_table = {}
    global_id_counter = 1
    global_errors = []

    def __init__(self, input_text):
        self.text = input_text
        self.pos = 0
        self.line = 1
        self.col = 1

    def peek(self):
        if self.pos < len(self.text):
            return self.text[self.pos]
        return ""

    def next_char(self):
        if self.pos < len(self.text):
            ch = self.text[self.pos]
            self.pos += 1
            if ch == '\n':
                self.line += 1
                self.col = 1
            else:
                self.col += 1
            return ch
        return ""

    @staticmethod
    def create_token(type, lexeme, start_line, start_col, end_line, end_col, attribute=None):
        return Token(type, lexeme, start_line, start_col, end_line, end_col, attribute)

    @staticmethod
    def error(message, start_line, start_col):
        error_msg = f"ERROR at ({start_line}, {start_col}): {message}"
        Lexer.global_errors.append(error_msg)
        print(error_msg, file=sys.stderr)

    def nextToken(self):
        while True:
            ch = self.peek()
            if ch == "":  # Если пустая строка, значит достигнут конец потока
                return self.create_token("EOF", "", self.line, self.col, self.line, self.col)
            if ch in [' ', '\t', '\n', '\r']:  # Пропускаем пробельные символы (пробел, табуляция, перевод строки)
                self.next_char()
            else:
                break

        start_line = self.line
        start_col = self.col
        ch = self.peek()

        if ch.isalpha():
            lexeme = ""
            while True:
                ch = self.peek()
                if ch != "" and ch.isalpha():
                    lexeme += self.next_char()
                else:
                    break
            if lexeme.lower() == "and":
                return self.create_token("AND", lexeme, start_line, start_col, self.line, self.col - 1)
            elif lexeme.lower() == "or":
                return self.create_token("OR", lexeme, start_line, start_col, self.line, self.col - 1)
            else:
                ident_lower = lexeme.lower()
                if ident_lower not in Lexer.global_id_table:
                    Lexer.global_id_table[ident_lower] = Lexer.global_id_counter
                    Lexer.global_id_counter += 1
                attr = Lexer.global_id_table[ident_lower]
                return self.create_token("IDENT", lexeme, start_line, start_col, self.line, self.col - 1,
                                         attribute=attr)

        if ch.isdigit():
            lexeme = ""
            if ch == '0':
                lexeme += self.next_char()  # Берём '0'
                next_ch = self.peek()
                if next_ch != "" and next_ch.lower() in ['b', 't', 'x']:
                    prefix = self.next_char()  # Берём префикс
                    lexeme += prefix
                    base = None
                    if prefix.lower() == 'b':
                        base = 2
                        valid_digits = "01"
                    elif prefix.lower() == 't':
                        base = 8
                        valid_digits = "01234567"
                    elif prefix.lower() == 'x':
                        base = 16
                        valid_digits = "0123456789abcdefABCDEF"
                    digits = ""
                    while True:
                        ch = self.peek()
                        if ch != "" and ch in valid_digits:
                            digits += self.next_char()
                        else:
                            break
                    if digits == "":
                        self.error(f"Неверный формат целого числа после префикса {lexeme}", start_line,
                                   start_col)
                        return self.create_token("ERROR", lexeme, start_line, start_col, self.line, self.col - 1)
                    lexeme += digits
                    try:
                        value = int(digits, base)
                    except ValueError:
                        self.error(f"Невозможно преобразовать {lexeme} в целое число", start_line, start_col)
                        return self.create_token("ERROR", lexeme, start_line, start_col, self.line, self.col - 1)
                    return self.create_token("INT", lexeme, start_line, start_col, self.line, self.col - 1,
                                             attribute=value)
                else:
                    digits = ""
                    while True:
                        ch = self.peek()
                        if ch != "" and ch.isdigit():
                            digits += self.next_char()
                        else:
                            break
                    lexeme += digits
                    try:
                        value = int(lexeme, 10)
                    except ValueError:
                        self.error(f"Невозможно преобразовать {lexeme} в целое число", start_line, start_col)
                        return self.create_token("ERROR", lexeme, start_line, start_col, self.line, self.col - 1)
                    return self.create_token("INT", lexeme, start_line, start_col, self.line, self.col - 1,
                                             attribute=value)
            else:
                digits = ""
                while True:
                    ch = self.peek()
                    if ch != "" and ch.isdigit():
                        digits += self.next_char()
                    else:
                        break
                lexeme += digits
                try:
                    value = int(lexeme, 10)
                except ValueError:
                    self.error(f"Невозможно преобразовать {lexeme} в целое число", start_line, start_col)
                    return self.create_token("ERROR", lexeme, start_line, start_col, self.line, self.col - 1)
                return self.create_token("INT", lexeme, start_line, start_col, self.line, self.col - 1,
                                         attribute=value)

        if ch == '(':
            lexeme = self.next_char()
            return self.create_token("LPAREN", lexeme, start_line, start_col, self.line, self.col - 1)
        if ch == ')':
            lexeme = self.next_char()
            return self.create_token("RPAREN", lexeme, start_line, start_col, self.line, self.col - 1)

        lexeme = self.next_char()
        self.error(f"Неожиданный символ: {lexeme}", start_line, start_col)
        return self.create_token("ERROR", lexeme, start_line, start_col, self.line, self.col - 1)


def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        try:
            with open(filename, 'r', encoding='utf-8') as f:
                input_text = f.read()
        except Exception as e:
            print(f"Ошибка чтения файла: {e}", file=sys.stderr)
            sys.exit(1)
        lexer = Lexer(input_text)
        token = lexer.nextToken()
        while token.type != "EOF":
            print(token)
            token = lexer.nextToken()
        print(token)
    else:
        print(
            "Введите текст для лексического анализа (в Windows для завершения ввода используйте CTRL+Z и Enter, "
            "для Linux — CTRL+D):")
        # Интерактивный режим: построчный разбор
        while True:
            try:
                line = sys.stdin.readline()
            except KeyboardInterrupt:
                break
            if line == "":
                break
            # Для каждой строки создаём отдельный анализатор, но таблица идентификаторов и ошибки сохраняются глобально
            lexer = Lexer(line)
            token = lexer.nextToken()
            while token.type != "EOF":
                print(token)
                token = lexer.nextToken()

    if Lexer.global_id_table:
        print("\nТаблица идентификаторов:")
        for ident, index in Lexer.global_id_table.items():
            print(f"{index}: {ident}")

    if Lexer.global_errors:
        print("\nОшибки лексического анализа:", file=sys.stderr)
        for err in Lexer.global_errors:
            print(err, file=sys.stderr)


if __name__ == "__main__":
    main()
