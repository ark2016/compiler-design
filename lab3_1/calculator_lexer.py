import re
from common import Token, ParseError, EOF_TOKEN_TYPE

def lex_calculator_tokens(text_content):
    # Терминалы, которые ожидает таблица для калькулятора: '+', '*', '(', ')', 'n'
    # Типы токенов должны им соответствовать.
    token_specification = [
        ('PLUS',      r'\+'),       # Будет токен Token(type='+', value='+',...)
        ('STAR',      r'\*'),       # Будет токен Token(type='*', value='*',...)
        ('LPAREN',    r'\('),      # Будет токен Token(type='(', value='(',...)
        ('RPAREN',    r'\)'),      # Будет токен Token(type=')', value=')',...)
        ('N_LITERAL', r'n'),       # Будет токен Token(type='n', value='n',...)
        # Если 'n' может быть числом, то нужно другое правило или приоритет
        # ('NUMBER',    r'[0-9]+'), # Если бы n было числом: Token(type='n', value='123',...)
        ('NEWLINE',   r'\n'),
        ('SKIP',      r'[ \t\r]+'),
        ('MISMATCH',  r'.'),
    ]
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    
    line_num = 1
    line_start_offset = 0
    tokens_found = []
    text_len = len(text_content)

    for mo in re.finditer(tok_regex, text_content):
        kind = mo.lastgroup
        value = mo.group()
        col = mo.start() - line_start_offset + 1

        if kind == 'NEWLINE':
            line_num += 1
            line_start_offset = mo.end()
            continue
        elif kind == 'SKIP':
            continue
        elif kind == 'MISMATCH':
            raise ParseError(f'Calculator Lexical Error: Unexpected char {value!r}', Token(kind, value, line_num, col))
        
        # Определяем тип токена, который пойдет в парсер (ключ для таблицы)
        token_type_for_parser = value # По умолчанию, сам символ является типом
        if kind == 'N_LITERAL':
            token_type_for_parser = 'n' 
        # elif kind == 'NUMBER': # Если бы 'n' означало число
        #     token_type_for_parser = 'n' 

        tokens_found.append(Token(token_type_for_parser, value, line_num, col))
            
    eof_line, eof_col = line_num, 1
    if tokens_found:
        last_tok = tokens_found[-1]
        eof_line = last_tok.line
        eof_col = last_tok.col + len(last_tok.value)
        if text_len > 0 :
            approx_last_tok_end_offset = line_start_offset + last_tok.col + len(last_tok.value) -1
            if approx_last_tok_end_offset < text_len:
                 remaining_text = text_content[approx_last_tok_end_offset:]
                 newlines_after = remaining_text.count('\n')
                 if newlines_after > 0:
                     eof_line += newlines_after
                     eof_col = len(remaining_text) - remaining_text.rfind('\n')
    elif text_content:
         eof_line = line_num
         if line_start_offset >= text_len: eof_col = 1
         else: eof_col = text_len - line_start_offset + 1

    tokens_found.append(Token(EOF_TOKEN_TYPE, '', eof_line, eof_col))
    return iter(tokens_found)