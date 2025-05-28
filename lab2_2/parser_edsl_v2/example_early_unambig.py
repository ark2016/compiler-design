import parser_edsl as pe


# Нетерминальные символы
Expr = pe.NonTerminal('Expr')

# Терминальные символы
INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)


# Правила грамматики
Expr |= Expr, '+', Expr, lambda x, y: x + y
Expr |= Expr, '-', Expr, lambda x, y: x - y
Expr |= INTEGER


# Парсер
parser = pe.Parser(Expr)
parser.add_skipped_domain('\\s')        # пробельные символы

print(parser.parse_earley('100 + 200'))
try:
    print(parser.parse_earley('100 + 200 - 150'))
except pe.ParseError as e:
    print('Ошибка:', e.pos, e.message)
