import re
from common import Token, ParseError, EOF_TOKEN_TYPE

def lex_gdl_tokens(text_content): 
    keywords = {'axiom', 'n'}
    token_specification = [
        ('COMMENT',   r'%[^\n]*(\n|\Z)'),
        ('LBRACK',    r'\['),
        ('RBRACK',    r'\]'),
        ('LPAREN',    r'\('),
        ('RPAREN',    r'\)'),
        ('OP',        r'[+\-*/]'),
        ('IDENT',     r'[A-Za-z][A-Za-z0-9_\']*'),
        ('NEWLINE',   r'\n'),
        ('SKIP',      r'[ \t\r]+'),
        ('MISMATCH',  r'.'),
    ]
    tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)
    
    line_num = 1
    line_start_offset = 0
    tokens_found = []
    text_len = len(text_content) # Для определения EOF

    for mo in re.finditer(tok_regex, text_content):
        kind = mo.lastgroup
        value = mo.group()
        col = mo.start() - line_start_offset + 1

        if kind == 'COMMENT':
            if '\n' in value: # Не только endswith, а в принципе содержит
                 line_num += value.count('\n')
                 # Обновляем line_start_offset до позиции после последнего \n в комментарии
                 last_newline_in_comment = value.rfind('\n')
                 if last_newline_in_comment != -1:
                     line_start_offset = mo.start() + last_newline_in_comment + 1
                 # Если комментарий не содержит \n, line_start_offset не меняется им
            continue
        elif kind == 'NEWLINE':
            line_num += 1
            line_start_offset = mo.end()
            continue
        elif kind == 'SKIP':
            continue
        elif kind == 'IDENT' and value in keywords:
            actual_kind = 'KW_AXIOM' if value == 'axiom' else 'KW_N'
            tokens_found.append(Token(actual_kind, value, line_num, col))
        elif kind == 'MISMATCH':
            raise ParseError(f'GDL Lexical Error: Unexpected character {value!r}', Token(kind, value, line_num, col))
        else:
            tokens_found.append(Token(kind, value, line_num, col))
            
    # Определение позиции EOF
    eof_line, eof_col = line_num, 1
    if tokens_found: # Если были токены
        last_tok = tokens_found[-1]
        eof_line = last_tok.line
        # Позиция EOF - после последнего символа последнего токена
        eof_col = last_tok.col + len(last_tok.value)
        
        # Проверка, были ли символы новой строки ПОСЛЕ последнего токена, но перед концом файла
        if text_len > 0 : # Только если текст не пустой
            start_search_for_eof_newlines = 0
            if hasattr(last_tok, 'value') and last_tok.value: # Убедимся что value есть и не пустое
                # Пытаемся найти индекс конца последнего токена в исходном тексте
                approx_last_tok_end_offset = line_start_offset + last_tok.col + len(last_tok.value) -1
                if approx_last_tok_end_offset < text_len:
                     remaining_text = text_content[approx_last_tok_end_offset:]
                     newlines_after = remaining_text.count('\n')
                     if newlines_after > 0:
                         eof_line += newlines_after
                         eof_col = len(remaining_text) - remaining_text.rfind('\n')

    elif text_content: # Текст есть, но токенов нет (только комментарии/пробелы)
        # line_num и line_start_offset должны отражать состояние после последнего обработанного символа
         eof_line = line_num
         # Если line_start_offset указывает на конец файла, то col = 1. Иначе, вычисляем.
         if line_start_offset >= text_len:
             eof_col = 1
         else:
             eof_col = text_len - line_start_offset + 1


    tokens_found.append(Token(EOF_TOKEN_TYPE, '', eof_line, eof_col))
    return iter(tokens_found)