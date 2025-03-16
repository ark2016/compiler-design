#!/usr/bin/env python3
# -*- coding: utf-8 -*-

NOSTATE = -1  # обозначение "нет перехода"

#
# 1) Факторизация алфавита (классы символов)
#
CLS_WS = 0  # Пробелы, \t, \n, \r
CLS_LETTER = 1  # Любая буква, кроме 'o' и 'c' (остальные)
CLS_DIGIT = 2  # Цифры 0-9
CLS_O = 3  # 'o'
CLS_C = 4  # 'c'
CLS_P = 5  # 'p'
CLS_E = 6  # 'e'
CLS_N = 7  # 'n'
CLS_L = 8  # 'l'
CLS_S = 9  # 's'
CLS_LT = 10  # '<'
CLS_GT = 11  # '>'
CLS_SQUOTE = 12  # одинарная кавычка: '
CLS_BSLASH = 13  # обратный слэш: \
CLS_EOF = 14  # условный класс "конец файла"
CLS_OTHER = 15  # все прочие символы

#
# 2) Таблица финальных состояний (индекс = номер состояния)
#
TOK_WHITESPACE = "WHITESPACE"
TOK_ID = "ID"
TOK_INT = "INT"
TOK_OPEN = "open"
TOK_CLOSE = "close"
TOK_LSHIFT = "<<"
TOK_RSHIFT = ">>"
TOK_STRING = "STRING"
TOK_EOF = "EOF"
TOK_UNKNOWN = "UNKNOWN"

# Согласно новому автомату (состояния q0..q20):
# Финальными являются: q1, q2, q3, q5, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q19, q20.
finals = [
    (False, TOK_UNKNOWN),  # 0: q0 (начальное)
    (True, TOK_WHITESPACE),  # 1: q1 (WHITESPACE)
    (True, TOK_ID),  # 2: q2 (ID)
    (True, TOK_INT),  # 3: q3 (INT)
    (False, TOK_UNKNOWN),  # 4: q4 (не финальное)
    (True, TOK_LSHIFT),  # 5: q5 ("<<")
    (False, TOK_UNKNOWN),  # 6: q6 (не финальное)
    (True, TOK_RSHIFT),  # 7: q7 (">>")
    (True, TOK_OPEN),  # 8: q8 (часть "open")
    (True, TOK_OPEN),  # 9: q9 (часть "open")
    (True, TOK_OPEN),  # 10: q10 (часть "open")
    (True, TOK_OPEN),  # 11: q11 ("open")
    (True, TOK_CLOSE),  # 12: q12 (часть "close")
    (True, TOK_CLOSE),  # 13: q13 (часть "close")
    (True, TOK_CLOSE),  # 14: q14 (часть "close")
    (True, TOK_CLOSE),  # 15: q15 (часть "close")
    (True, TOK_CLOSE),  # 16: q16 ("close")
    (False, TOK_UNKNOWN),  # 17: q17 (внутри строкового литерала)
    (False, TOK_UNKNOWN),  # 18: q18 (после \ в строке)
    (True, TOK_STRING),  # 19: q19 (строковый литерал)
    (True, TOK_EOF)  # 20: q20 (EOF)
]

#
# 3) Матрица переходов delta[state][class] = next_state
#    Столбцы соответствуют классам:
#    [CLS_WS, CLS_LETTER, CLS_DIGIT, CLS_O, CLS_C, CLS_P, CLS_E, CLS_N, CLS_L, CLS_S, CLS_LT, CLS_GT, CLS_SQUOTE, CLS_BSLASH, CLS_EOF, CLS_OTHER]
#
delta = [
    # 0 (q0): начальное состояние S
    [1, 2, 3, 8, 12, 2, 2, 2, 2, 2, 4, 6, 17, -1, 20, -1],
    # 1 (q1 - WHITESPACE): повторять пробелы
    [1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
    # 2 (q2 - ID): остаёмся в ID при букве или цифре
    [-1, 2, 2, 2, 2, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 3 (q3 - INT): остаёмся в INT при цифрах
    [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
    # 4 (q4): получено '<'; ждем второй '<'
    [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1],
    # 5 (q5 - "<<"): финальное, без переходов
    [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
    # 6 (q6): получено '>'; ждем второй '>'
    [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7, -1, -1, -1, -1],
    # 7 (q7 - ">>"): финальное, без переходов
    [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
    # 8 (q8): получено "o" -> если 'p' то q9, иначе буква/цифра → q2
    [-1, 2, 2, 2, 2, 9, 2, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 9 (q9): "op" -> если 'e' то q10, иначе буква/цифра → q2
    [-1, 2, 2, 2, 2, 2, 10, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 10 (q10): "ope" -> если 'n' then q11, иначе буква/цифра → q2
    [-1, 2, 2, 2, 2, 2, 2, 11, 2, 2, -1, -1, -1, -1, -1, -1],
    # 11 (q11 - open): после "open" → если буква или цифра, переходим в ID (q2)
    [-1, 2, 2, 2, 2, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 12 (q12): получено "c" -> если 'l' (CLS_L) то q13, иначе буква/цифра → q2
    [-1, 2, 2, 2, 2, 2, 2, 2, 13, 2, -1, -1, -1, -1, -1, -1],
    # 13 (q13): "cl" -> если 'o' (CLS_O) then q14, иначе буква/цифра → q2
    [-1, 2, 2, 14, 2, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 14 (q14): "clo" -> если 's' (CLS_S) then q15, иначе буква/цифра → q2
    [-1, 2, 2, 2, 2, 2, 2, 2, 2, 15, -1, -1, -1, -1, -1, -1],
    # 15 (q15): "clos" -> если 'e' (CLS_E) then q16, иначе буква/цифра → q2
    [-1, 2, 2, 2, 2, 2, 16, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 16 (q16 - close): после "close" → если буква/цифра, переходим в ID (q2)
    [-1, 2, 2, 2, 2, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1, -1],
    # 17 (q17): внутри строкового литерала
    # Если символ НЕ одинарная кавычка (CLS_SQUOTE) и не обратный слэш (CLS_BSLASH) → остаёмся в q17,
    # для CLS_SQUOTE переходим в q19, для CLS_BSLASH → q18; для EOF – ошибка.
    [17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 19, 18, -1, 17],
    # 18 (q18): после обратного слэша в строке → любой символ возвращает в q17; для EOF – ошибка.
    [17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, -1, 17],
    # 19 (q19 - STRING): финальное, без переходов
    [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
    # 20 (q20 - EOF): финальное, без переходов
    [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1]
]


def char_to_class(ch: str) -> int:
    """
    Отображаем реальный символ в индекс класса (0..15).
    При пустой строке ('') интерпретируем как конец файла.
    """
    if ch == '':
        return CLS_EOF
    if ch in (' ', '\t', '\n', '\r'):
        return CLS_WS
    elif ch.isdigit():
        return CLS_DIGIT
    elif ch == 'o':
        return CLS_O
    elif ch == 'c':
        return CLS_C
    elif ch == 'p':
        return CLS_P
    elif ch == 'e':
        return CLS_E
    elif ch == 'n':
        return CLS_N
    elif ch == 'l':
        return CLS_L
    elif ch == 's':
        return CLS_S
    elif ch == '<':
        return CLS_LT
    elif ch == '>':
        return CLS_GT
    elif ch == "'":
        return CLS_SQUOTE
    elif ch == '\\':
        return CLS_BSLASH
    elif ch.isalpha():
        return CLS_LETTER
    else:
        return CLS_OTHER


class Token:
    def __init__(self, ttype, lexeme, line, col):
        self.type = ttype
        self.lexeme = lexeme
        self.line = line
        self.col = col

    def __str__(self):
        return f"{self.type}({self.line}:{self.col}): {self.lexeme}"


class DFALexer:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.line = 1
        self.col = 1

    def _peek(self):
        """Вернуть текущий символ или '' (пустая строка), если конец."""
        if self.pos >= len(self.text):
            return ''
        return self.text[self.pos]

    def _advance(self):
        """Сдвинуться на один символ и вернуть его.
           Если достигнут конец, возвращается ''.
        """
        ch = self._peek()
        if ch != '':
            self.pos += 1
            if ch == '\n':
                self.line += 1
                self.col = 1
            else:
                self.col += 1
        return ch

    def next_token(self) -> Token:
        """
        Считывает одну лексему с применением правила:
        "При попадании в заключительное состояние запоминаем координату.
         Если попадаем в ловушку, откатываемся к последнему заключительному состоянию".
        Реализовано методом максимального совпадения (maximal munch).
        """
        start_line = self.line
        start_col = self.col
        initial_pos = self.pos

        current_state = 0  # q0 (начальное)
        lexeme_chars = []
        # Запоминаем последнее финальное состояние:
        last_final_state = None
        last_final_lexeme = []
        last_final_pos = (self.pos, self.line, self.col)

        while True:
            ch = self._peek()
            cls_ = char_to_class(ch)
            if current_state < 0 or current_state >= len(delta):
                break  # критическая ошибка
            row = delta[current_state]
            if cls_ < 0 or cls_ >= len(row):
                break  # ошибка: класс вне диапазона
            next_state = row[cls_]
            if next_state == NOSTATE:
                break  # не можем перейти дальше
            else:
                # Переход возможен – запоминаем символ и позицию.
                current_state = next_state
                ch = self._advance()
                lexeme_chars.append(ch)
                # Если новое состояние является заключительным – запоминаем его:
                is_final, token_type = finals[current_state]
                if is_final:
                    last_final_state = current_state
                    last_final_lexeme = lexeme_chars.copy()
                    last_final_pos = (self.pos, self.line, self.col)
                # Если состояние EOF, выходим
                if current_state == 20:
                    break

        # Если не нашли ни одного заключительного состояния, возвращаем ошибку:
        if last_final_state is None:
            bad_char = self._advance()
            if bad_char == '':
                bad_char = "EOF"
            return Token(TOK_UNKNOWN, bad_char, self.line, self.col)
        else:
            # Откатываемся к последней запомненной позиции
            self.pos, self.line, self.col = last_final_pos
            token_type = finals[last_final_state][1]
            token_lexeme = ''.join(last_final_lexeme)
            # Если это WHITESPACE – пропускаем и считываем следующий токен
            if token_type == TOK_WHITESPACE:
                return self.next_token()
            else:
                return Token(token_type, token_lexeme, start_line, start_col)

    def tokenize_all(self):
        """Возвращает список всех токенов до появления TOK_EOF."""
        tokens = []
        while True:
            t = self.next_token()
            tokens.append(t)
            if t.type == TOK_EOF:
                break
        return tokens


if __name__ == "__main__":
    # Пример теста
    sample_text = """open 123 close
    << 'some string with \\' quote' >> openx
c close123
"""
    lexer = DFALexer(sample_text)
    all_tokens = lexer.tokenize_all()
    for token in all_tokens:
        print(token)
