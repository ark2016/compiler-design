# generic_ll1_parser.py
from common import Node, Token, ParseError, EPSILON, EOF_TOKEN_TYPE # Импортируем необходимые сущности

class GenericLL1Parser:
    def __init__(self, lexer_func, parse_table_module):
        self.lexer_func = lexer_func # Функция лексического анализатора
        
        # Загружаем информацию о грамматике из модуля таблицы
        self.parse_table = parse_table_module.parse_table
        self.axiom = parse_table_module.GRAMMAR_AXIOM
        self.terminals = parse_table_module.GRAMMAR_TERMINALS
        self.non_terminals = parse_table_module.GRAMMAR_NON_TERMINALS
        
        # Используем константы EPSILON и EOF_TOKEN_TYPE из загруженного модуля,
        # чтобы быть консистентными с тем, как они были сохранены генератором таблиц
        self.EPSILON = parse_table_module.EPSILON
        self.EOF_TOKEN_TYPE = parse_table_module.EOF_TOKEN_TYPE
        
        self.tokens_iter = None
        self.current_token = None
        self.tree_root = None
        self.parsing_stack = []

    def _next_token(self):
        try:
            self.current_token = next(self.tokens_iter)
        except StopIteration:
            # Если current_token уже был, используем его координаты для EOF
            line = self.current_token.line if self.current_token and hasattr(self.current_token, 'line') else 1
            col = (self.current_token.col + len(self.current_token.value)) \
                  if self.current_token and hasattr(self.current_token, 'col') and hasattr(self.current_token, 'value') and self.current_token.value \
                  else 1
            self.current_token = Token(self.EOF_TOKEN_TYPE, '', line, col)

    def _error(self, message):
        # Формируем информацию о стеке для диагностики
        stack_symbols = [item[0] for item in reversed(self.parsing_stack)] # item[0] - это символ
        detail = f"{message}. Stack (top to bottom): {stack_symbols}"
        raise ParseError(detail, self.current_token)

    def parse(self, text_to_parse):
        Node._ids = 0 # Сбрасываем счетчик ID узлов для каждого нового разбора
        self.tokens_iter = self.lexer_func(text_to_parse) # Вызываем лексер
        self._next_token() # Получаем первый токен

        self.tree_root = Node(self.axiom) # Корень дерева разбора - аксиома
        # Стек: (символ_на_обработку, узел_родитель_в_дереве_для_этого_символа)
        # Первый элемент на обработку - аксиома. Ее родитель в дереве - None (она сама корень).
        # Но узел для аксиомы уже создан (self.tree_root), поэтому ее "дети" будут добавляться к нему.
        # В стек кладем аксиому, ее "результат" (узел) - это self.tree_root.
        # А символы ее правой части будут иметь self.tree_root в качестве родителя.
        # Изначальный стек для парсера: (EOF, None), (АКСИОМА, None_или_узел_для_АКСИОМЫ_если_он_уже_есть)
        self.parsing_stack = [(self.EOF_TOKEN_TYPE, self.tree_root), (self.axiom, self.tree_root)]
        # Важно: parent_for_tree_node_children для аксиомы должен быть self.tree_root,
        # чтобы ее дети (символы из первой продукции) добавлялись к нему.
        # Когда мы извлекаем ('AXIOM', self.tree_root), current_tree_node становится self.tree_root.

        while self.parsing_stack:
            stack_top_symbol, parent_for_new_nodes = self.parsing_stack.pop()

            if stack_top_symbol == self.EOF_TOKEN_TYPE:
                if self.current_token.type == self.EOF_TOKEN_TYPE:
                    # EOF на стеке и EOF на входе - успешное завершение
                    # Добавляем узел EOF к родителю (который был для аксиомы, т.е. self.tree_root)
                    if parent_for_new_nodes: # Это должен быть self.tree_root
                         parent_for_new_nodes.add_child(Node(self.EOF_TOKEN_TYPE, token=self.current_token))
                    print(f"GenericLL1Parser: Parsing successful for axiom '{self.axiom}'!")
                    return self.tree_root
                else:
                    self._error(f"Expected {self.EOF_TOKEN_TYPE} but found '{self.current_token.type}'")
            
            current_processed_node = None # Узел в дереве, соответствующий stack_top_symbol

            if stack_top_symbol in self.terminals: # Это терминал языка, который парсим
                if stack_top_symbol == self.current_token.type: # Сравниваем по ТИПУ токена
                    current_processed_node = Node(self.current_token.type, token=self.current_token)
                    if parent_for_new_nodes:
                        parent_for_new_nodes.add_child(current_processed_node)
                    else: # Терминал без родителя в дереве - странно, но возможно для корневого терминала (не наш случай)
                        print(f"Warning: Terminal '{stack_top_symbol}' matched without parent node in tree.")
                    self._next_token()
                else:
                    self._error(f"Expected terminal '{stack_top_symbol}' but found token type '{self.current_token.type}' (value: '{self.current_token.value}')")
            
            elif stack_top_symbol in self.non_terminals: # Это нетерминал
                # Создаем узел для этого нетерминала в дереве разбора
                # Исключение: если это сама аксиома, ее узел уже self.tree_root
                if stack_top_symbol == self.axiom and parent_for_new_nodes == self.tree_root:
                    # Это первый вход для аксиомы, ее узел уже self.tree_root
                    current_processed_node = self.tree_root
                else:
                    current_processed_node = Node(stack_top_symbol)
                    if parent_for_new_nodes:
                        parent_for_new_nodes.add_child(current_processed_node)
                    else: # Не должно случиться для не-аксиомных NT, если стек правильно инициализирован
                        self._error(f"Internal parser error: NT '{stack_top_symbol}' created without parent in tree.")
                
                # Ищем правило в таблице: table[Нетерминал][Тип_токена_предосмотра]
                nt_rules_for_lookahead = self.parse_table.get(stack_top_symbol)
                if not nt_rules_for_lookahead: # Должно быть обработано в grammar_analyzer
                    self._error(f"Non-terminal '{stack_top_symbol}' not found as a key in the parse table.")

                # Тип текущего токена используем как ключ для терминала
                production_to_apply = nt_rules_for_lookahead.get(self.current_token.type)
                if production_to_apply is None:
                    expected_one_of = list(nt_rules_for_lookahead.keys())
                    self._error(f"No production rule in table for NT '{stack_top_symbol}' on lookahead token type '{self.current_token.type}'. Expected one of: {expected_one_of}")
                
                _lhs_from_table, rhs_symbols_from_table = production_to_apply

                if rhs_symbols_from_table == [self.EPSILON]:
                    current_processed_node.add_child(Node(self.EPSILON))
                else:
                    # Добавляем символы правой части в стек в обратном порядке
                    # Их родительским узлом в дереве будет current_processed_node (узел для текущего NT)
                    for symbol_in_rhs in reversed(rhs_symbols_from_table):
                        self.parsing_stack.append((symbol_in_rhs, current_processed_node))
            
            elif stack_top_symbol == self.EPSILON: 
                # Эпсилон не должен быть на стеке как символ для обработки.
                # Он добавляется в дерево как дочерний узел.
                # Если он тут, значит что-то пошло не так при добавлении в стек.
                 pass # Пропускаем, если он как-то попал на стек. Узел уже должен быть добавлен.
            else: # Символ на стеке не является ни T, ни NT, ни ε
                self._error(f"Unknown symbol '{stack_top_symbol}' encountered on parsing stack. Not in terminals or non-terminals.")
        
        # Если цикл закончился, а мы не дошли до return self.tree_root (EOF/EOF)
        self._error("Parsing ended unexpectedly (e.g., stack empty but not EOF).")