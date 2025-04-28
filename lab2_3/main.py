import re
from collections import namedtuple
import sys
import argparse
import os

Token = namedtuple('Token', ['type', 'value', 'line', 'col'])

def lex(text):
    keywords = {'axiom', 'n'}
    token_specification = [
        # Комментарий (от '%' до конца строки, с опциональным переносом строки)
        ('COMMENT',   r'%[^\n]*\n?'),
        ('LBRACK',    r'\['),                # [
        ('RBRACK',    r'\]'),                # ]
        ('LPAREN',    r'\('),                # (
        ('RPAREN',    r'\)'),                # )
        # Операторы
        ('OP',        r'[+\-*/]'),           # +,-,*,/
        # Идентификаторы (латинская буква, затем буквы/апострофы)
        ('IDENT',     r'[A-Za-z][A-Za-z\']*'),
        # Перенос строки (обрабатываем отдельно для подсчета строк)
        ('NEWLINE',   r'\n'),
        # Пробельные символы (пропускаем: пробел, табуляция, возврат каретки)
        ('SKIP',      r'[ \t\r]+'),
        # Любой другой символ (ошибка лексического анализа)
        ('MISMATCH',  r'.'),
    ]
    # Комбинированное регулярное выражение с именованными группами
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    line_num = 1 # Текущий номер строки
    line_start = 0 # Индекс начала текущей строки во всем тексте
    mo_iter = re.finditer(tok_regex, text) 

    while True:
        try:
            mo = next(mo_iter)
            kind = mo.lastgroup # Тип совпавшего токена (имя группы)
            value = mo.group()  # Значение совпавшего текста
            col = mo.start() - line_start + 1 

            if kind == 'COMMENT':
                if '\n' in value:
                   line_num += value.count('\n')
                   line_start = mo.end() 
            elif kind == 'NEWLINE':
                line_start = mo.end() 
                line_num += 1
            elif kind == 'SKIP':
                # Пропускаем пробельные символы
                pass
            elif kind == 'IDENT' and value in keywords:
                 # Если IDENT совпал с ключевым словом, присваиваем соответствующий тип
                 if value == 'axiom':
                     kind = 'KW_AXIOM'
                 elif value == 'n':
                     kind = 'KW_N'
                 # Генерируем токен ключевого слова
                 yield Token(kind, value, line_num, col)
            elif kind == 'MISMATCH':
                # Неожиданный символ - лексическая ошибка
                raise RuntimeError(f'Lexical Error: Unexpected character {value!r} at line {line_num}, col {col}')
            else:
                # Генерируем токен для всех остальных типов
                yield Token(kind, value, line_num, col)
        except StopIteration:
            # Достигнут конец входного текста
            break

    eof_col = len(text) - line_start + 1 if text else 1
    yield Token('EOF', '', line_num, eof_col)

class Node:
    _ids = 0 # Счетчик для генерации уникальных ID узлов для Graphviz
    def __init__(self, symbol, token=None):
        self.symbol = symbol 
        self.token = token   
        self.children = []  
        # Уникальный ID для узла (используется в Graphviz)
        self.id = f"n{Node._ids}"
        Node._ids += 1

    def add_child(self, node):
        if node:
            self.children.append(node)

    def __repr__(self):
        # Представление узла для отладки
        if self.token:
            return f"Node({self.token.type}='{self.token.value}', id={self.id})"
        else:
            return f"Node({self.symbol}, id={self.id})"

class ParseError(Exception):
    pass

class PredictiveParser:
    def __init__(self, tokens):
        self.tokens = iter(tokens)
        self.current_token = None
        self.root_node = Node("PROG")
        self.stack = [('EOF', None), ('PROG', self.root_node)]
        self._next_token()

    def _next_token(self):
        try:
            self.current_token = next(self.tokens)
        except StopIteration:
            # Если токены закончились раньше, чем ожидался EOF, ставим EOF токен-заглушку
            self.current_token = Token('EOF', '', -1, -1)

    def _error(self, expected):
        if self.current_token and self.current_token.type != 'EOF':
            token_info = f"'{self.current_token.value}' (type {self.current_token.type})"
            pos_info = f"at line {self.current_token.line}, col {self.current_token.col}"
        else:
            token_info = "end of input"
            pos_info = ""
        stack_info = [s[0] for s in reversed(self.stack)]
        raise ParseError(f"Syntax Error: Expected {expected}, but found {token_info} {pos_info}. Stack context (top->): {stack_info}")

    def _match(self, expected_token_type, parent_node_for_result):
        """
        Проверяет, соответствует ли текущий токен ожидаемому типу.
        Если да, потребляет токен и добавляет его как терминальный узел к родителю.
        Если нет, генерирует ошибку.
        """
        if self.current_token.type == expected_token_type:
            token = self.current_token
            terminal_node = Node(token.type, token=token)
            if parent_node_for_result:
                 parent_node_for_result.add_child(terminal_node)
            else:
                 print(f"Warning: Matched terminal {token.type} with no parent node.")
            self._next_token()
            return terminal_node 
        else:
            self._error(f"token of type {expected_token_type}")

    def parse(self):
        """
        Основной цикл предсказывающего разбора.
        Работает, пока стек не опустеет (или не останется только EOF).
        Построение дерева разбора интегрировано.
        """
        print("Starting parse...")
        Node._ids = 0
        self.root_node = Node("PROG")
        self.stack = [('EOF', None), ('PROG', self.root_node)]
        
        while self.stack:
            top_symbol, parent_for_result = self.stack.pop()

            if top_symbol == self.current_token.type:
                self._match(top_symbol, parent_for_result)
                continue

            # --- EOF Match ---
            if top_symbol == 'EOF':
                 if self.current_token.type == 'EOF':
                    print("Parsing successful!")
                    eof_node = Node('EOF', token=self.current_token)
                    if self.root_node:
                         self.root_node.add_child(eof_node)
                    else:
                         print("Warning: Cannot add EOF node, root_node is None.", file=sys.stderr)
                    return self.root_node
                 else:
                    self._error("end of input (EOF)")

            # --- Non-Terminal ---
            current_node = Node(top_symbol)
            if parent_for_result:
                parent_for_result.add_child(current_node)
            elif top_symbol == 'PROG':
                current_node = self.root_node
            else:
                print(f"Warning: Creating node for {top_symbol} without a parent during expansion.")

            lookahead_type = self.current_token.type

            # --- Реализация логики таблицы предсказывающего разбора (через if/elif) ---

            # --- Строка таблицы для нетерминала PROG ---
            # PROG ::= (COMMENT | WS)* AXIOM (COMMENT | WS)* GRAMMAR (COMMENT | WS)*
            if top_symbol == 'PROG':
                 # В таблице: M[PROG, '['] = -> AXIOM GRAMMAR
                 # Проверяем столбец: ожидаем '[' (начало AXIOM)
                 if lookahead_type == 'LBRACK':
                     # Применяем правило PROG -> AXIOM GRAMMAR
                     # Помещаем символы правой части правила в стек в обратном порядке
                     # Указываем `current_node` (узел PROG) как родителя для AXIOM и GRAMMAR
                     self.stack.append(('GRAMMAR', current_node))
                     self.stack.append(('AXIOM', current_node))
                 else:
                     # Ошибка: для PROG возможно только начать с AXIOM ([)
                     self._error("start of AXIOM ('[')")

            # --- Строка таблицы для нетерминала AXIOM ---
            # AXIOM ::= WS* '[' WS* 'axiom' WS* '[' WS* IDENT WS* ']' WS* ']' WS*
            elif top_symbol == 'AXIOM':
                 # В таблице: M[AXIOM, '['] = -> [ axiom [ IDENT ] ]
                 # Проверяем столбец: ожидаем '['
                 if lookahead_type == 'LBRACK':
                     # Применяем правило AXIOM -> [ axiom [ IDENT ] ]
                     # Помещаем символы правой части в стек в обратном порядке.
                     # parent_for_result для терминалов и нетерминалов RHS - это `current_node` (узел AXIOM).
                     self.stack.append(('RBRACK', current_node))      # ]
                     self.stack.append(('RBRACK', current_node))      # ]
                     self.stack.append(('IDENT', current_node))       # IDENT (тип токена)
                     self.stack.append(('LBRACK', current_node))      # [
                     self.stack.append(('KW_AXIOM', current_node))    # axiom (тип токена)
                     self.stack.append(('LBRACK', current_node))      # [
                 else:
                     # Ошибка: AXIOM должен начинаться с '['
                     self._error("'[' to start AXIOM definition")

            # --- Строка таблицы для нетерминала GRAMMAR ---
            # GRAMMAR ::= (WS* RULES WS*)*
            # Преобразовано для LL(1): GRAMMAR ::= RULES GRAMMAR | ε
            elif top_symbol == 'GRAMMAR':
                 # M[GRAMMAR, '['] = -> RULES GRAMMAR
                 # Проверяем столбец: ожидаем '[' (начало RULES)
                 if lookahead_type == 'LBRACK':
                     # GRAMMAR -> RULES GRAMMAR
                     # Помещаем в стек. current_node (узел GRAMMAR) - родитель для RULES и GRAMMAR.
                     self.stack.append(('GRAMMAR', current_node)) # Рекурсивный вызов GRAMMAR
                     self.stack.append(('RULES', current_node))   # Начало RULES
                 # M[GRAMMAR, EOF] = -> ε
                 # Проверяем столбец: ожидаем EOF (конец файла, Follow(GRAMMAR))
                 elif lookahead_type == 'EOF':
                     # Применяем правило GRAMMAR -> ε
                     # Удаляем GRAMMAR со стека (уже сделали pop). Добавляем узел ε в дерево.
                     current_node.add_child(Node('ε'))
                 else:
                     # Ошибка: для GRAMMAR возможно либо начать RULES ([), либо закончиться (EOF)
                     self._error("start of RULES ('[') or EOF")

            # --- Строка таблицы для нетерминала RULES ---
            # RULES ::= WS* '[' WS* IDENT ( WS* RULE )+ WS* ']' WS*
            # Преобразовано для LL(1): RULES ::= '[' IDENT RULE RULE_TAIL ']'
            elif top_symbol == 'RULES':
                 # В таблице: M[RULES, '['] = -> [ IDENT RULE RULE_TAIL ]
                 # Проверяем столбец: ожидаем '['
                 if lookahead_type == 'LBRACK':
                     # Применяем правило RULES -> [ IDENT RULE RULE_TAIL ]
                     self.stack.append(('RBRACK', current_node))   # ]
                     self.stack.append(('RULE_TAIL', current_node)) # RULE_TAIL (для обработки RULE+)
                     self.stack.append(('RULE', current_node))      # Первый RULE
                     self.stack.append(('IDENT', current_node))     # IDENT
                     self.stack.append(('LBRACK', current_node))   # [
                 else:
                     # Ошибка: RULES должен начинаться с '['
                     self._error("'[' to start RULES definition")

            # --- Строка таблицы для нетерминала RULE_TAIL ---
            # RULE_TAIL ::= RULE RULE_TAIL | ε (для обработки RULE+)
            elif top_symbol == 'RULE_TAIL':
                 # В таблице: M[RULE_TAIL, '['] = -> RULE RULE_TAIL
                 # Проверяем столбец: ожидаем '[' (начало еще одного RULE)
                 if lookahead_type == 'LBRACK':
                     # Применяем правило RULE_TAIL -> RULE RULE_TAIL
                     self.stack.append(('RULE_TAIL', current_node)) # Рекурсивный вызов RULE_TAIL
                     self.stack.append(('RULE', current_node))      # Очередной RULE
                 # В таблице: M[RULE_TAIL, ']'] = -> ε
                 # Проверяем столбец: ожидаем ']' (конец RULES, Follow(RULE_TAIL))
                 elif lookahead_type == 'RBRACK':
                     # Применяем правило RULE_TAIL -> ε
                     current_node.add_child(Node('ε')) # Добавляем узел ε
                 else:
                     # Ошибка: RULE_TAIL может начаться с RULE ([) или закончиться (])
                     self._error("start of another RULE ('[') or end of RULES (']')")

            # RULE ::= WS* '[' WS* OP WS+ IDENT WS+ IDENT WS* ']' WS*
            #      | WS* '[' WS* IDENT WS+ IDENT WS* ']' WS*
            #      | WS* '[' WS* ']' WS*
            #      | WS* '[' WS* 'n' WS* ']' WS*
            #      | WS* '[' WS* '(' WS* IDENT WS* ')' WS* ']' WS*
            elif top_symbol == 'RULE':
                 # Все правила RULE начинаются с '['. Это конфликт для LL(1).
                 # В таблице: M[RULE, '['] = См. Особое правило
                 # Проверяем столбец: ожидаем '['
                 if lookahead_type == 'LBRACK':
                     # --- Шаги Особого правила для M[RULE, LBRACK]: ---
                     # 1. Сопоставить и потребить терминал '['. Он добавляется как дочерний к текущему узлу RULE.
                     self._match('LBRACK', current_node)
                     # 2. Посмотреть на *следующий* токен (после потребленного '['). Это второй токен предпросмотра.
                     token_after_lbrack_type = self.current_token.type
                     # 3. Все правила RULE заканчиваются на ']', добавляем его в стек первым (в обратном порядке).
                     self.stack.append(('RBRACK', current_node))

                     # 4. На основе типа токена *после* '[' решаем, какую правую часть правила RULE выбрать и добавить в стек.
                     # Случай: t_next == OP -> [ OP IDENT IDENT ]
                     if token_after_lbrack_type == 'OP':
                          self.stack.append(('IDENT', current_node))
                          self.stack.append(('IDENT', current_node))
                          self.stack.append(('OP', current_node)) # OP - это тип терминала
                     # Случай: t_next == IDENT -> [ IDENT IDENT ]
                     elif token_after_lbrack_type == 'IDENT':
                          self.stack.append(('IDENT', current_node))
                          self.stack.append(('IDENT', current_node))
                     # Случай: t_next == RBRACK -> [ ] (пустое правило)
                     elif token_after_lbrack_type == 'RBRACK':
                          # Правая часть пустая, кроме '[' и ']', которые уже обработаны/в стеке.
                          # Добавляем узел ε для обозначения пустого тела правила.
                          current_node.add_child(Node('ε'))
                     # Случай: t_next == KW_N -> [ n ]
                     elif token_after_lbrack_type == 'KW_N':
                          self.stack.append(('KW_N', current_node)) # KW_N - тип терминала
                     # Случай: t_next == LPAREN -> [ ( IDENT ) ]
                     elif token_after_lbrack_type == 'LPAREN':
                           self.stack.append(('RPAREN', current_node)) # ) - тип терминала
                           self.stack.append(('IDENT', current_node))  # IDENT - тип терминала
                           self.stack.append(('LPAREN', current_node)) # ( - тип терминала
                     else:
                          # Ошибка: неожидаемый токен после '[' внутри RULE
                          self._error("OP, IDENT, ']', 'n', or '(' inside RULE definition")
                 else:
                     # Ошибка: RULE должен начинаться с '['
                     self._error("'[' to start RULE definition")

            # --- Обработка символов, не покрытых таблицей (резервная ошибка) ---
            else:
                 self._error(f"processing unknown stack symbol '{top_symbol}' with lookahead '{lookahead_type}'")

        # --- Проверки после завершения цикла ---
        # Если стек пуст, но остались токены во входном потоке (кроме EOF)
        if self.current_token.type != 'EOF':
             self._error("end of input (EOF) - unexpected tokens remain")
        else:
             print("Warning: Parsing loop finished, but EOF condition wasn't met as expected inside loop.", file=sys.stderr)
             return None 

def generate_dot(root_node):
    if not root_node:
        return "digraph G {}"

    dot_nodes = []
    dot_edges = []
    rank_constraints = []

    queue = [root_node]
    processed_nodes = set()

    while queue:
        current_node = queue.pop(0)
        if current_node.id in processed_nodes:
            continue
        processed_nodes.add(current_node.id)

        label = _get_node_label(current_node)
        dot_nodes.append(f'  {current_node.id} [label="{label}"];')

        child_ids = []
        valid_children = [child for child in current_node.children if child]

        for child in valid_children:
            dot_edges.append(f"  {current_node.id} -> {child.id};")
            child_ids.append(child.id)
            if child.id not in processed_nodes:
                queue.append(child)

        if len(child_ids) > 1:
             invisible_edges = " -> ".join(child_ids)
             rank_constraints.append(f"  {{ rank=same; {invisible_edges} [style=invis] }}")

    dot_string = "digraph {\n"
    dot_string += "  node [shape=ellipse];\n"
    dot_string += "\n".join(dot_nodes) + "\n"
    dot_string += "\n".join(dot_edges) + "\n"
    dot_string += "\n".join(rank_constraints) + "\n"
    dot_string += "}"
    return dot_string

def _get_node_label(node):
    def escape(s):
        return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')

    if node.symbol == 'ε':
         return "ε"
    if node.symbol == 'EOF':
        return "EOF"

    if node.token:
        val = escape(node.token.value)
        type_esc = escape(node.token.type)

        if type_esc in ['LBRACK', 'RBRACK', 'LPAREN', 'RPAREN', 'OP']:
             return f"{val}"
        elif type_esc == 'KW_AXIOM':
             return "axiom"
        elif type_esc == 'KW_N':
             return "n"
        elif type_esc == 'IDENT':
             return f"{val}"
        else:
             return f"{type_esc}\\n({val})"
    else:
        return escape(node.symbol)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Predictive parser for grammar definition language. Generates DOT output.")
    parser.add_argument("input_file", help="Path to the input grammar file.")
    parser.add_argument("-o", "--output", help="Path to the output DOT file (optional, prints to stdout if omitted).")

    args = parser.parse_args()

    try:
        print(f"Reading input from: {args.input_file}")
        with open(args.input_file, 'r', encoding='utf-8') as f:
            input_grammar_text = f.read()

        print("Lexing input...")
        tokens = list(lex(input_grammar_text))

        print("Initializing parser...")
        parser_instance = PredictiveParser(tokens)
        parse_tree_root = parser_instance.parse()

        if parse_tree_root:
            print("Generating DOT output...")
            dot_output = generate_dot(parse_tree_root)
            if args.output:
                output_path = args.output
                print(f"Writing DOT output to: {output_path}")
                with open(output_path, 'w', encoding='utf-8') as f_out:
                    f_out.write(dot_output)
                print(f"DOT output successfully written.")
            else:
                print("\n--- DOT Output (stdout) ---")
                print(dot_output)
                print("--- End DOT Output ---")

    except FileNotFoundError:
        print(f"Error: Input file not found: {args.input_file}", file=sys.stderr)
        sys.exit(1)
    except (RuntimeError, ParseError) as e:
        print(f"\nError encountered during processing:\n{e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"\nAn unexpected error occurred: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)