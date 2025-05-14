from common import Node, Token, ParseError, EPSILON, EOF_TOKEN_TYPE

class GDLHardcodedParser:
    def __init__(self, tokens_iterator): # Принимает итератор токенов GDL
        self.tokens = tokens_iterator
        self.current_token = None
        self.root_node = None # Будет создан в parse()
        self._next_token()

    def _next_token(self):
        try:
            self.current_token = next(self.tokens)
        except StopIteration:
            # Если current_token уже был, используем его координаты для EOF
            line = self.current_token.line if self.current_token else 1
            col = (self.current_token.col + len(self.current_token.value)) if self.current_token and self.current_token.value else 1
            # Важно, чтобы тип EOF токена совпадал с тем, что ожидает логика парсера
            self.current_token = Token(EOF_TOKEN_TYPE, '', line, col) 

    def _error(self, expected_description):
        raise ParseError(f"GDL Parser Error: Expected {expected_description}", self.current_token)

    def _match(self, expected_token_type, parent_node_for_result):
        if self.current_token.type == expected_token_type:
            token = self.current_token
            terminal_node = Node(token.type, token=token)
            if parent_node_for_result:
                 parent_node_for_result.add_child(terminal_node)
            self._next_token()
            return terminal_node
        else:
            self._error(f"token of type '{expected_token_type}'")

    def parse(self):
        Node._ids = 0 
        self.root_node = Node("PROG") # Аксиома GDL грамматики
        # Стек: (символ_для_обработки, узел_родитель_в_дереве)
        self.parsing_stack = [(EOF_TOKEN_TYPE, self.root_node), ('PROG', None)] 

        while self.parsing_stack:
            top_symbol, parent_for_tree_node_children = self.parsing_stack.pop()

            if top_symbol == EOF_TOKEN_TYPE:
                 if self.current_token.type == EOF_TOKEN_TYPE:
                    if parent_for_tree_node_children: # Добавляем EOF к PROG-узлу
                        parent_for_tree_node_children.add_child(Node(EOF_TOKEN_TYPE, token=self.current_token))
                    return self.root_node 
                 else:
                    self._error(f"end of input ('{EOF_TOKEN_TYPE}')")
            
            gdl_terminals = ['LBRACK', 'RBRACK', 'LPAREN', 'RPAREN', 'OP', 'IDENT', 'KW_AXIOM', 'KW_N']
            if top_symbol in gdl_terminals:
                self._match(top_symbol, parent_for_tree_node_children)
                continue

            # Это нетерминал GDL
            current_gdl_nt_node = None
            if top_symbol == 'PROG' and parent_for_tree_node_children is None: 
                current_gdl_nt_node = self.root_node 
            else:
                current_gdl_nt_node = Node(top_symbol) 
                if parent_for_tree_node_children:
                    parent_for_tree_node_children.add_child(current_gdl_nt_node)
                else: 
                    raise ParseError(f"Internal GDL parser error: NT {top_symbol} has no parent for its node.")

            lookahead_gdl_token_type = self.current_token.type
            
            # LL(1) разбор для GDL             
            if top_symbol == 'PROG': # PROG ::= AXIOM GRAMMAR EOF
                 if lookahead_gdl_token_type == 'LBRACK': 
                     rhs = [('AXIOM', current_gdl_nt_node), ('GRAMMAR', current_gdl_nt_node), (EOF_TOKEN_TYPE, current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs)) 
                 else: self._error("start of AXIOM ('[') for PROG rule")
            elif top_symbol == 'AXIOM': # AXIOM ::= '[' KW_AXIOM '[' IDENT ']' ']'
                 if lookahead_gdl_token_type == 'LBRACK':
                     rhs = [
                         ('LBRACK', current_gdl_nt_node), ('KW_AXIOM', current_gdl_nt_node), ('LBRACK', current_gdl_nt_node),
                         ('IDENT', current_gdl_nt_node), ('RBRACK', current_gdl_nt_node), ('RBRACK', current_gdl_nt_node)
                     ]
                     self.parsing_stack.extend(reversed(rhs))
                 else: self._error("'[' to start AXIOM definition")
            elif top_symbol == 'GRAMMAR': # GRAMMAR ::= RULE_DEFINITION GRAMMAR | ε 
                 if lookahead_gdl_token_type == 'LBRACK': 
                     rhs = [('RULE_DEFINITION', current_gdl_nt_node), ('GRAMMAR', current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs))
                 elif lookahead_gdl_token_type == EOF_TOKEN_TYPE: 
                     current_gdl_nt_node.add_child(Node(EPSILON))
                 else: self._error("start of RULE_DEFINITION ('[') or EOF for GRAMMAR rule")
            elif top_symbol == 'RULE_DEFINITION': # RULE_DEFINITION ::= '[' IDENT RULE_BODIES ']'
                 if lookahead_gdl_token_type == 'LBRACK':
                     rhs = [('LBRACK', current_gdl_nt_node), ('IDENT', current_gdl_nt_node), 
                            ('RULE_BODIES', current_gdl_nt_node), ('RBRACK', current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs))
                 else: self._error("'[' to start RULE_DEFINITION")
            elif top_symbol == 'RULE_BODIES': # RULE_BODIES ::= RULE_BODY RULE_BODIES_TAIL
                 if lookahead_gdl_token_type == 'LBRACK': 
                     rhs = [('RULE_BODY', current_gdl_nt_node), ('RULE_BODIES_TAIL', current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs))
                 else: self._error("'[' to start RULE_BODY in RULE_BODIES")
            elif top_symbol == 'RULE_BODIES_TAIL': # RULE_BODIES_TAIL ::= RULE_BODY RULE_BODIES_TAIL | ε
                 if lookahead_gdl_token_type == 'LBRACK': 
                     rhs = [('RULE_BODY', current_gdl_nt_node), ('RULE_BODIES_TAIL', current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs))
                 elif lookahead_gdl_token_type == 'RBRACK': 
                     current_gdl_nt_node.add_child(Node(EPSILON))
                 else: self._error("start of another RULE_BODY ('[') or end of RULE_DEFINITION (']') for RULE_BODIES_TAIL")
            elif top_symbol == 'RULE_BODY': # RULE_BODY ::= '[' RHS_SYMBOLS ']'
                 if lookahead_gdl_token_type == 'LBRACK':
                     rhs = [('LBRACK', current_gdl_nt_node), ('RHS_SYMBOLS', current_gdl_nt_node), ('RBRACK', current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs))
                 else: self._error("'[' to start RULE_BODY")
            elif top_symbol == 'RHS_SYMBOLS': # RHS_SYMBOLS ::= SYMBOL RHS_SYMBOLS | ε
                 if lookahead_gdl_token_type in ['IDENT', 'OP', 'LPAREN', 'RPAREN', 'KW_N']:
                     rhs = [('SYMBOL', current_gdl_nt_node), ('RHS_SYMBOLS', current_gdl_nt_node)]
                     self.parsing_stack.extend(reversed(rhs))
                 elif lookahead_gdl_token_type == 'RBRACK': 
                     current_gdl_nt_node.add_child(Node(EPSILON))
                 else: self._error("IDENT, OP, '(', ')', 'n', or ']' for RHS_SYMBOLS rule")
            elif top_symbol == 'SYMBOL': 
                 # Эти правила не рекурсивные, просто добавляем один символ (вершина стека)
                 terminal_to_push = None
                 if lookahead_gdl_token_type == 'IDENT': terminal_to_push = 'IDENT'
                 elif lookahead_gdl_token_type == 'OP': terminal_to_push = 'OP'
                 elif lookahead_gdl_token_type == 'LPAREN': terminal_to_push = 'LPAREN'
                 elif lookahead_gdl_token_type == 'RPAREN': terminal_to_push = 'RPAREN'
                 elif lookahead_gdl_token_type == 'KW_N': terminal_to_push = 'KW_N'
                 
                 if terminal_to_push:
                     self.parsing_stack.append((terminal_to_push, current_gdl_nt_node))
                 else:
                     self._error("IDENT, OP, '(', ')', or 'n' for SYMBOL rule")
            else:
                 self._error(f"Unknown non-terminal '{top_symbol}' in GDL parser.")
        
        self._error("GDL parsing ended unexpectedly.")