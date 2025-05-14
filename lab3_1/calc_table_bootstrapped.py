# Generated Parse Table and Grammar Info
EPSILON = "ε"
EOF_TOKEN_TYPE = "EOF"
GRAMMAR_AXIOM = "E"
GRAMMAR_TERMINALS = {'n', '+', '(', '*', 'EOF', ')'}
GRAMMAR_NON_TERMINALS = {'E', 'F', 'T', 'E_prime', 'T_prime'}
parse_table = {   'E': {'(': ('E', ['T', 'E_prime']), 'n': ('E', ['T', 'E_prime'])},
    'E_prime': {')': ('E_prime', ['ε']), '+': ('E_prime', ['+', 'T', 'E_prime']), 'EOF': ('E_prime', ['ε'])},
    'F': {'(': ('F', ['(', 'E', ')']), 'n': ('F', ['n'])},
    'T': {'(': ('T', ['F', 'T_prime']), 'n': ('T', ['F', 'T_prime'])},
    'T_prime': {   ')': ('T_prime', ['ε']),
                   '*': ('T_prime', ['*', 'F', 'T_prime']),
                   '+': ('T_prime', ['ε']),
                   'EOF': ('T_prime', ['ε'])}}
