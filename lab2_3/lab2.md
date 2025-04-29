```
PROG ::=  AXIOM GRAMMAR EOF 

AXIOM ::=  '[' KW_AXIOM '[' IDENT ']' ']'

GRAMMAR ::=  RULE_DEFINITION GRAMMAR |  ε                   

RULE_DEFINITION ::=  '[' IDENT RULE_BODIES ']'

RULE_BODIES  ::=  RULE_BODY RULE_BODIES_TAIL

RULE_BODIES_TAIL ::=  RULE_BODY RULE_BODIES_TAIL |  ε

RULE_BODY ::=  '[' RHS_SYMBOLS ']'

RHS_SYMBOLS ::=  SYMBOL RHS_SYMBOLS |  ε

SYMBOL ::=  IDENT
      |  OP
      |  LPAREN  // '('
      |  RPAREN  // ')'
      |  KW_N    // 'n'
```