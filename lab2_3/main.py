import re
from collections import namedtuple
import sys
import argparse
import os

Token = namedtuple('Token', ['type', 'value', 'line', 'col'])

def lex(text):
    keywords = {'axiom', 'n'}
    token_specification = [
        ('COMMENT',   r'%[^\n]*\n?'),
        ('LBRACK',    r'\['),
        ('RBRACK',    r'\]'),
        ('LPAREN',    r'\('),
        ('RPAREN',    r'\)'),
        ('OP',        r'[+\-*/]'),
        ('IDENT',     r'[A-Za-z][A-Za-z\']*'),
        ('NEWLINE',   r'\n'),
        ('SKIP',      r'[ \t\r]+'),
        ('MISMATCH',  r'.'),
    ]
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    line_num = 1
    line_start = 0
    mo_iter = re.finditer(tok_regex, text)

    while True:
        try:
            mo = next(mo_iter)
            kind = mo.lastgroup
            value = mo.group()
            col = mo.start() - line_start + 1

            if kind == 'COMMENT':
                if '\n' in value:
                   line_num += value.count('\n')
                   line_start = mo.end()
                else:
                   pass
            elif kind == 'NEWLINE':
                line_start = mo.end()
                line_num += 1
            elif kind == 'SKIP':
                pass
            elif kind == 'IDENT' and value in keywords:
                 if value == 'axiom':
                     kind = 'KW_AXIOM'
                 elif value == 'n':
                     kind = 'KW_N'
                 yield Token(kind, value, line_num, col)
            elif kind == 'MISMATCH':
                raise RuntimeError(f'Lexical Error: Unexpected character {value!r} at line {line_num}, col {col}')
            else:
                yield Token(kind, value, line_num, col)
        except StopIteration:
            break

    eof_col = len(text) - line_start + 1 if text else 1
    yield Token('EOF', '', line_num, eof_col)

class Node:
    _ids = 0
    def __init__(self, symbol, token=None):
        self.symbol = symbol
        self.token = token
        self.children = []
        self.id = f"n{Node._ids}"
        Node._ids += 1

    def add_child(self, node):
        if node:
            self.children.append(node)

    def __repr__(self):
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
        self._advance()

    def _advance(self):
        try:
            self.current_token = next(self.tokens)
        except StopIteration:
            self.current_token = Token('EOF', '', -1, -1)

    def _error(self, expected):
        if self.current_token and self.current_token.type != 'EOF':
            token_info = f"'{self.current_token.value}' (type {self.current_token.type})"
            pos_info = f"at line {self.current_token.line}, col {self.current_token.col}"
        else:
            token_info = "end of input"
            pos_info = ""
        stack_info = [s[0] for s in self.stack]
        raise ParseError(f"Syntax Error: Expected {expected}, but found {token_info} {pos_info}. Stack context: {stack_info}")

    def _match(self, expected_token_type, parent_node_for_result):
        if self.current_token.type == expected_token_type:
            token = self.current_token
            terminal_node = Node(token.type, token=token)
            if parent_node_for_result:
                 parent_node_for_result.add_child(terminal_node)
            else:
                 print(f"Warning: Matched terminal {token.type} with no parent node.")
            self._advance()
            return terminal_node
        else:
            self._error(f"token of type {expected_token_type}")

    def parse(self):
        print("Starting parse...")
        Node._ids = 0
        self.root_node = Node("PROG")
        self.stack = [('EOF', None), ('PROG', self.root_node)]


        while self.stack:
            top_symbol, parent_for_result = self.stack.pop()

            # --- Terminal Match ---
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
            # Create node for non-terminal
            current_node = Node(top_symbol)
            if parent_for_result:
                parent_for_result.add_child(current_node)
            elif top_symbol == 'PROG':
                current_node = self.root_node 
            else:
                print(f"Warning: Creating node for {top_symbol} without a parent during expansion.")

            lookahead_type = self.current_token.type

            # --- Table Logic Implementation ---

            # --- Row: PROG ---
            if top_symbol == 'PROG':
                 # Column: [
                 if lookahead_type == 'LBRACK':
                     # M[PROG, '['] = -> AXIOM GRAMMAR
                     self.stack.append(('GRAMMAR', current_node))
                     self.stack.append(('AXIOM', current_node))
                 else:
                     # Error: Other columns empty for PROG
                     self._error("start of AXIOM ('[')")

            # --- Row: AXIOM ---
            elif top_symbol == 'AXIOM':
                 # Column: [
                 if lookahead_type == 'LBRACK':
                     # M[AXIOM, '['] = -> [ axiom [ IDENT ] ]
                     self.stack.append(('RBRACK', current_node))
                     self.stack.append(('RBRACK', current_node))
                     self.stack.append(('IDENT', current_node))
                     self.stack.append(('LBRACK', current_node))
                     self.stack.append(('KW_AXIOM', current_node))
                     self.stack.append(('LBRACK', current_node))
                 else:
                     # Error: Other columns empty for AXIOM
                     self._error("'[' to start AXIOM definition")

            # --- Row: GRAMMAR ---
            elif top_symbol == 'GRAMMAR':
                 # Column: [
                 if lookahead_type == 'LBRACK':
                     # M[GRAMMAR, '['] = -> RULES GRAMMAR
                     self.stack.append(('GRAMMAR', current_node))
                     self.stack.append(('RULES', current_node))
                 # Column: EOF
                 elif lookahead_type == 'EOF':
                     # M[GRAMMAR, EOF] = -> ε
                     current_node.add_child(Node('ε'))
                 else:
                     # Error: Other columns empty for GRAMMAR
                     self._error("start of RULES ('[') or EOF")

            # --- Row: RULES ---
            elif top_symbol == 'RULES':
                 # Column: [
                 if lookahead_type == 'LBRACK':
                     # M[RULES, '['] = -> [ IDENT RULE RULE_TAIL ]
                     self.stack.append(('RBRACK', current_node))
                     self.stack.append(('RULE_TAIL', current_node))
                     self.stack.append(('RULE', current_node))
                     self.stack.append(('IDENT', current_node))
                     self.stack.append(('LBRACK', current_node))
                 else:
                     # Error: Other columns empty for RULES
                     self._error("'[' to start RULES definition")

            # --- Row: RULE_TAIL ---
            elif top_symbol == 'RULE_TAIL':
                 # Column: [
                 if lookahead_type == 'LBRACK':
                     # M[RULE_TAIL, '['] = -> RULE RULE_TAIL
                     self.stack.append(('RULE_TAIL', current_node))
                     self.stack.append(('RULE', current_node))
                 # Column: ]
                 elif lookahead_type == 'RBRACK':
                     # M[RULE_TAIL, ']'] = -> ε
                     current_node.add_child(Node('ε'))
                 else:
                     # Error: Other columns empty for RULE_TAIL
                     self._error("start of another RULE ('[') or end of RULES (']')")

            # --- Row: RULE (Special Handling) ---
            elif top_symbol == 'RULE':
                 # Column: [ (Unique action for this cell)
                 if lookahead_type == 'LBRACK':
                     # 1. Match and consume '['
                     self._match('LBRACK', current_node)
                     # 2. Look at the *next* token (t_next)
                     token_after_lbrack_type = self.current_token.type
                     self.stack.append(('RBRACK', current_node))

                     # 3. Decide based on t_next and push corresponding symbols
                     # Case: t_next == OP
                     if token_after_lbrack_type == 'OP':
                          self.stack.append(('IDENT', current_node))
                          self.stack.append(('IDENT', current_node))
                          self.stack.append(('OP', current_node))
                     # Case: t_next == IDENT
                     elif token_after_lbrack_type == 'IDENT':
                          self.stack.append(('IDENT', current_node))
                          self.stack.append(('IDENT', current_node))
                     # Case: t_next == RBRACK (Empty rule body)
                     elif token_after_lbrack_type == 'RBRACK':
                          current_node.add_child(Node('ε')) # Add epsilon node
                     # Case: t_next == KW_N
                     elif token_after_lbrack_type == 'KW_N':
                          self.stack.append(('KW_N', current_node))
                     # Case: t_next == LPAREN
                     elif token_after_lbrack_type == 'LPAREN':
                           self.stack.append(('RPAREN', current_node))
                           self.stack.append(('IDENT', current_node))
                           self.stack.append(('LPAREN', current_node))
                     else:
                          # Error case within the special rule
                          self._error("OP, IDENT, ']', 'n', or '(' inside RULE definition")
                 else:
                     # Error: RULE must start with '['
                     self._error("'[' to start RULE definition")

            # --- Fallback Error ---
            else:
                 # Should not happen if all non-terminals are handled above
                 self._error(f"processing unknown stack symbol '{top_symbol}'")

        # --- Post-Loop Checks ---
        if self.current_token.type != 'EOF':
             self._error("end of input (EOF) - unexpected tokens remain")
        else:
             print("Warning: Parsing loop finished, but EOF condition wasn't met as expected.", file=sys.stderr)
             return None 
        Ы
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