% Лабораторная работа № 0.0. Знакомство с компиляцией программ
% 11 февраля 2025 г.
% Аркадий Лебедев, ИУ9-61Б

# Цель работы
Заполнить первую лабораторную, которая проводится до лекции.

# Индивидуальный вариант
Требуется написать парсер FORTH-подобного языка, описанный в индивидуальном варианте. 
Парсер должен строить синтаксическое дерево в виде вложенных массивов (списков) 
выбранного языка программирования, слова изображаются строками, числа — числами, 
управляющие конструкции — вложенными списками.Требуется написать парсер FORTH-подобного языка, 
описанный в индивидуальном варианте. Парсер должен строить синтаксическое дерево в виде вложенных 
массивов (списков) выбранного языка программирования, слова изображаются строками, числа — числами,
управляющие конструкции — вложенными списками.

Грамматика:
```
<Program>  ::= <Articles> <Body> .
<Articles> ::= <Article> <Articles> | .
<Article>  ::= define word <Body> end .
<Body>     ::= if <Body> <ElsePart> endif <Body>
             | integer <Body> | word <Body> | .
<ElsePart> ::= else <Body> | .
```

# Реализация

```python
class ParseError(Exception):
    pass

_reserved_tokens = {"endif", "else", "end", "define"}

def tokenize(s):
    tokens = s.split()
    result = []
    for tok in tokens:
        try:
            n = int(tok)
            result.append(n)
        except ValueError:
            result.append(tok)
    return result

def consume_token(tokens, expected):
    if not tokens:
        raise ParseError(f"Неожиданный конец ввода; ожидался токен '{expected}'")
    token = tokens[0]
    if token != expected:
        raise ParseError(f"Ожидался токен '{expected}', а получен '{token}'")
    return tokens[1:]

def parse_program(tokens):
    articles, tokens = parse_articles(tokens)
    main_body, tokens = parse_body(tokens, stop_tokens=[])
    if tokens:
        raise ParseError("Обнаружены лишние токены после завершения программы")
    return [articles, main_body]

def parse_articles(tokens):
    articles = {}
    while tokens and tokens[0] == "define":
        name, body, tokens = parse_article(tokens)
        if name in articles:
            raise ParseError(f"Дублирование определения: {name}")
        articles[name] = body
    return articles, tokens

def parse_article(tokens):
    tokens = consume_token(tokens, "define")
    if not tokens:
        raise ParseError("Ожидалось имя Article после 'define'")
    name = tokens[0]
    if isinstance(name, int):
        raise ParseError("Имя Article не может быть числом")
    tokens = tokens[1:]
    body, tokens = parse_body(tokens, stop_tokens=["end"])
    tokens = consume_token(tokens, "end")
    return name, body, tokens

def parse_body(tokens, stop_tokens):
    instructions = []
    while tokens:
        if tokens[0] in stop_tokens:
            break
        if not stop_tokens and tokens[0] in _reserved_tokens:
            if tokens[0] != "if":
                raise ParseError(f"Неожиданный зарезервированный токен: {tokens[0]}")
        if tokens[0] == "if":
            instr, tokens = parse_if(tokens)
        else:
            instr, tokens = parse_simple(tokens)
        instructions.append(instr)
    return instructions, tokens

def parse_simple(tokens):
    if not tokens:
        raise ParseError("Неожиданный конец ввода при разборе простого токена")
    token = tokens[0]
    return token, tokens[1:]

def parse_if(tokens):
    tokens = consume_token(tokens, "if")
    then_branch, tokens = parse_body(tokens, stop_tokens=["else", "endif"])
    if tokens and tokens[0] == "else":
        tokens = consume_token(tokens, "else")
        else_branch, tokens = parse_body(tokens, stop_tokens=["endif"])
        tokens = consume_token(tokens, "endif")
        return ["if", then_branch, "else", else_branch], tokens
    else:
        tokens = consume_token(tokens, "endif")
        return ["if", then_branch], tokens

def parse(s):
    tokens = tokenize(s)
    try:
        tree = parse_program(tokens)
        return tree
    except ParseError as e:
        print(f"Ошибка разбора: {e}")
        return None

def run_tests():
    input_str = "define abs dup 0 < if -1 * endif end 10 abs -10 abs"
    expected = [{"abs": ["dup", 0, "<", ["if", [-1, "*"]]]}, [10, "abs", -10, "abs"]]
    result = parse(input_str)
    assert result == expected, f"Test 1 failed: got {result}, expected {expected}"

    input_str2 = ""
    expected2 = [{}, []]
    result2 = parse(input_str2)
    assert result2 == expected2, f"Test 2 failed: got {result2}, expected {expected2}"

    input_str3 = "dup 10 swap"
    expected3 = [{}, ["dup", 10, "swap"]]
    result3 = parse(input_str3)
    assert result3 == expected3, f"Test 3 failed: got {result3}, expected {expected3}"

    input_str4 = "if 1 else 2 endif"
    expected4 = [{}, [["if", [1], "else", [2]]]]
    result4 = parse(input_str4)
    assert result4 == expected4, f"Test 4 failed: got {result4}, expected {expected4}"

    input_str5 = "if if 1 endif endif"
    expected5 = [{}, [["if", [["if", [1]]]]]]
    result5 = parse(input_str5)
    assert result5 == expected5, f"Test 5 failed: got {result5}, expected {expected5}"

    input_str6 = "define inc 1 + end 5 inc"
    expected6 = [{"inc": [1, "+"]}, [5, "inc"]]
    result6 = parse(input_str6)
    assert result6 == expected6, f"Test 6 failed: got {result6}, expected {expected6}"

    input_str7 = "define foo dup"
    result7 = parse(input_str7)
    assert result7 is None, f"Test 7 failed: expected None для синтаксической ошибки, got {result7}"

    input_str8 = "dup endif"
    result8 = parse(input_str8)
    assert result8 is None, f"Test 8 failed: expected None для синтаксической ошибки, got {result8}"

    input_str9 = "1 2 +"
    expected9 = [{}, [1, 2, "+"]]
    result9 = parse(input_str9)
    assert result9 == expected9, f"Test 9 failed: got {result9}, expected {expected9}"

    input_str10 = "x dup 0 swap if drop -1 endif"
    expected10 = [{}, ["x", "dup", 0, "swap", ["if", ["drop", -1]]]]
    result10 = parse(input_str10)
    assert result10 == expected10, f"Test 10 failed: got {result10}, expected {expected10}"

    input_str11 = "x dup 0 swap if drop -1 else swap 1 + endif"
    expected11 = [{}, ["x", "dup", 0, "swap", ["if", ["drop", -1], "else", ["swap", 1, "+"]]]]
    result11 = parse(input_str11)
    assert result11 == expected11, f"Test 11 failed: got {result11}, expected {expected11}"

    input_str12 = (
        "define -- 1 - end "
        "define =0? dup 0 = end "
        "define =1? dup 1 = end "
        "define factorial "
            " =0? if drop 1 exit endif "
            " =1? if drop 1 exit endif "
            " dup -- factorial * "
        "end "
        "0 factorial 1 factorial 2 factorial 3 factorial 4 factorial"
    )
    expected12 = [
        {
            "--": [1, "-"],
            "=0?": ["dup", 0, "="],
            "=1?": ["dup", 1, "="],
            "factorial": [
                "=0?", ["if", ["drop", 1, "exit"]],
                "=1?", ["if", ["drop", 1, "exit"]],
                "dup", "--", "factorial", "*"
            ]
        },
        [0, "factorial", 1, "factorial", 2, "factorial", 3, "factorial", 4, "factorial"]
    ]
    result12 = parse(input_str12)
    assert result12 == expected12, f"Test 12 failed: got {result12}, expected {expected12}"

    input_str13 = (
        "define -- 1 - end "
        "define =0? dup 0 = end "
        "define =1? dup 1 = end "
        "define factorial "
            " =0? if drop 1 else =1? if drop 1 else dup -- factorial * endif endif "
        "end "
        "0 factorial 1 factorial 2 factorial 3 factorial 4 factorial"
    )
    expected13 = [
        {
            "--": [1, "-"],
            "=0?": ["dup", 0, "="],
            "=1?": ["dup", 1, "="],
            "factorial": [
                "=0?",
                ["if", ["drop", 1], "else", [
                    "=1?",
                    ["if", ["drop", 1], "else", ["dup", "--", "factorial", "*"]]
                ]]
            ]
        },
        [0, "factorial", 1, "factorial", 2, "factorial", 3, "factorial", 4, "factorial"]
    ]
    result13 = parse(input_str13)
    assert result13 == expected13, f"Test 13 failed: got {result13}, expected {expected13}"

    input_str14 = "define word w1 w2 w3"
    result14 = parse(input_str14)
    assert result14 is None, f"Test 14 failed: expected None для синтаксической ошибки, got {result14}"

    print("Все тесты пройдены.")

if __name__ == '__main__':
    run_tests()
```

# Тестирование

```python
>>> parse("define abs dup 0 < if -1 * endif end 10 abs -10 abs")
[{"abs": ["dup", 0, "<", ["if", [-1, "*"]]]}, [10, "abs", -10, "abs"]]
>>> parse("")
[{}, []]
>>> parse("dup 10 swap")
[{}, ["dup", 10, "swap"]]
>>> parse("if 1 else 2 endif")
[{}, [["if", [1], "else", [2]]]]
>>> parse("if if 1 endif endif")
[{}, [["if", [["if", [1]]]]]]
>>> parse("define inc 1 + end 5 inc")
[{"inc": [1, "+"]}, [5, "inc"]]
>>> parse("define foo dup")
None
>>> parse("dup endif")
None
>>> parse("1 2 +")
[{}, [1, 2, "+"]]
>>> parse("x dup 0 swap if drop -1 endif")
[{}, ["x", "dup", 0, "swap", ["if", ["drop", -1]]]]
>>> parse("x dup 0 swap if drop -1 else swap 1 + endif")
[{}, ["x", "dup", 0, "swap", ["if", ["drop", -1], "else", ["swap", 1, "+"]]]]
>>> parse("define -- 1 - end define =0? dup 0 = end define =1? dup 1 = end define factorial =0? if drop 1 exit endif =1? if drop 1 exit endif dup -- factorial * end 0 factorial 1 factorial 2 factorial 3 factorial 4 factorial")
[{"--": [1, "-"], "=0?": ["dup", 0, "="], "=1?": ["dup", 1, "="], "factorial": ["=0?", ["if", ["drop", 1, "exit"]], "=1?", ["if", ["drop", 1, "exit"]], "dup", "--", "factorial", "*"]}, [0, "factorial", 1, "factorial", 2, "factorial", 3, "factorial", 4, "factorial"]]
>>> parse("define -- 1 - end define =0? dup 0 = end define =1? dup 1 = end define factorial =0? if drop 1 else =1? if drop 1 else dup -- factorial * endif endif end 0 factorial 1 factorial 2 factorial 3 factorial 4 factorial")
[{"--": [1, "-"], "=0?": ["dup", 0, "="], "=1?": ["dup", 1, "="], "factorial": ["=0?", ["if", ["drop", 1], "else", ["=1?", ["if", ["drop", 1], "else", ["dup", "--", "factorial", "*"]]]]]}, [0, "factorial", 1, "factorial", 2, "factorial", 3, "factorial", 4, "factorial"]]
>>> parse("define word w1 w2 w3")
None
```

# Вывод
В ходе выполнения лабораторной работы я научился разрабатывать парсер для FORTH-подобного языка, который строит синтаксическое дерево в виде вложенных списков. Я также научился обрабатывать синтаксические ошибки и тестировать парсер на различных примерах.