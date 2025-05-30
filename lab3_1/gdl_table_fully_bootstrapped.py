# Generated Parse Table and Grammar Info
EPSILON = "ε"
EOF_TOKEN_TYPE = "EOF"
GRAMMAR_AXIOM = "PROG"
GRAMMAR_TERMINALS = {'EOF',
 'IDENT',
 'KW_AXIOM',
 'KW_N',
 'LBRACK',
 'LPAREN',
 'OP',
 'RBRACK',
 'RPAREN'}
GRAMMAR_NON_TERMINALS = {'AXIOM',
 'GRAMMAR',
 'PROG',
 'RHS_SYMBOLS',
 'RULE_BODIES',
 'RULE_BODIES_TAIL',
 'RULE_BODY',
 'RULE_DEFINITION',
 'SYMBOL'}
parse_table = {   'AXIOM': {'LBRACK': ('AXIOM', ['LBRACK', 'KW_AXIOM', 'LBRACK', 'IDENT', 'RBRACK', 'RBRACK'])},
    'GRAMMAR': {'EOF': ('GRAMMAR', ['ε']), 'LBRACK': ('GRAMMAR', ['RULE_DEFINITION', 'GRAMMAR'])},
    'PROG': {'LBRACK': ('PROG', ['AXIOM', 'GRAMMAR', 'EOF'])},
    'RHS_SYMBOLS': {   'IDENT': ('RHS_SYMBOLS', ['SYMBOL', 'RHS_SYMBOLS']),
                       'KW_N': ('RHS_SYMBOLS', ['SYMBOL', 'RHS_SYMBOLS']),
                       'LPAREN': ('RHS_SYMBOLS', ['SYMBOL', 'RHS_SYMBOLS']),
                       'OP': ('RHS_SYMBOLS', ['SYMBOL', 'RHS_SYMBOLS']),
                       'RBRACK': ('RHS_SYMBOLS', ['ε']),
                       'RPAREN': ('RHS_SYMBOLS', ['SYMBOL', 'RHS_SYMBOLS'])},
    'RULE_BODIES': {'LBRACK': ('RULE_BODIES', ['RULE_BODY', 'RULE_BODIES_TAIL'])},
    'RULE_BODIES_TAIL': {   'LBRACK': ('RULE_BODIES_TAIL', ['RULE_BODY', 'RULE_BODIES_TAIL']),
                            'RBRACK': ('RULE_BODIES_TAIL', ['ε'])},
    'RULE_BODY': {'LBRACK': ('RULE_BODY', ['LBRACK', 'RHS_SYMBOLS', 'RBRACK'])},
    'RULE_DEFINITION': {'LBRACK': ('RULE_DEFINITION', ['LBRACK', 'IDENT', 'RULE_BODIES', 'RBRACK'])},
    'SYMBOL': {   'IDENT': ('SYMBOL', ['IDENT']),
                  'KW_N': ('SYMBOL', ['KW_N']),
                  'LPAREN': ('SYMBOL', ['LPAREN']),
                  'OP': ('SYMBOL', ['OP']),
                  'RPAREN': ('SYMBOL', ['RPAREN'])}}
