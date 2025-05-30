import parser_edsl as pe
import re
import math


CONSTANTS = {
    'pi': math.pi,
    'e': math.e,
    'Na': 6.02214076e23, # Постоянная Больцмана
    'kB': 1.380649e-23,  # Число Авогадро
}


# Нетерминальные символы
Expr = pe.NonTerminal('Expr')
Term = pe.NonTerminal('Term')
Factor = pe.NonTerminal('Factor')


# Функция вычисления атрибута
def bounded_int(value):
    ivalue = int(value)
    if ivalue < 65536:
        return ivalue
    else:
        raise pe.TokenAttributeError(f'Слишком большое число: ' + value)

# Терминальные символы
INTEGER = pe.Terminal('INTEGER', '[0-9]+', bounded_int, priority=7)
REAL = pe.Terminal('REAL', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
CONST = pe.Terminal('CONST', '[A-Za-z]+', str)
KW_MOD = pe.Terminal('mod', 'mod', lambda name: None,
                     re_flags=re.IGNORECASE, priority=10)


# Правила грамматики
Expr |= Expr, '+', Term, lambda x, y: x + y
Expr |= Expr, '-', Term, lambda x, y: x - y
Expr |= Term

Term |= Term, '*', Factor, lambda x, y: x * y
Term |= Term, '/', Factor, lambda x, y: x / y
Term |= Term, KW_MOD, Factor, lambda x, y: x % y
Term |= Factor

Factor |= INTEGER
Factor |= '(', Expr, ')'
Factor |= REAL
Factor |= CONST, lambda name: CONSTANTS[name]


# Парсер
parser = pe.Parser(Expr)

parser.add_skipped_domain('\\s')        # пробельные символы
parser.add_skipped_domain('\\{.*?\\}')  # комментарии вида {...}

print(parser.parse_earley('21 * (1 + { комментарий } 1)'))
print('Универсальная газовая постоянная =', parser.parse_earley('Na * kB'))
print(parser.parse_earley('100 mod 7 + 100 MOD 13'))

try:
    bad_expr = '7 + 100500'
    print('Пример лексической ошибки:', bad_expr)
    print(parser.parse_earley(bad_expr))
except pe.Error as e:
    print('Ошибка:', e.pos, e.message)

for token in parser.tokenize('mod Mod MOD mood 100 {комментарий} 100. 1e2'):
    print(token.pos, token)
