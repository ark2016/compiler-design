import enum
import abc
import typing
import dataclasses
from dataclasses import dataclass
import re
import sys

import parser_edsl as pe

# Определение абстрактного синтаксического дерева

# Класс для методов семантического действия
@dataclass
class MethodSignature:
    return_type: str
    name: str
    parameters: list[tuple[str, bool]]  # (тип, is_array)
    
    def __str__(self):
        params = []
        for param_type, is_array in self.parameters:
            params.append(f"{param_type}{'[]' if is_array else ''}")
        return f"{self.return_type} {self.name}({', '.join(params)})"

# Класс для отображения типов
@dataclass
class TypeMapping:
    identifiers: list[str]
    type_name: str
    is_array: bool = False
    
    def __str__(self):
        return f"{', '.join(self.identifiers)}: {self.type_name}{'[]' if self.is_array else ''}"

# Элементы правила грамматики
class RuleElement(abc.ABC):
    pass

@dataclass
class Terminal(RuleElement):
    name: str
    
    def __str__(self):
        return self.name

@dataclass
class NonTerminal(RuleElement):
    name: str
    
    def __str__(self):
        return self.name

@dataclass
class Repetition(RuleElement):
    element: RuleElement
    
    def __str__(self):
        return f"%rep {self.element}"

@dataclass
class NestedRule(RuleElement):
    alternatives: list['RuleAlternative']
    
    def __str__(self):
        return f"({' | '.join(str(alt) for alt in self.alternatives)})"

# Альтернатива правила
@dataclass
class RuleAlternative:
    elements: list[RuleElement]
    semantic_action: str = None
    
    def __str__(self):
        result = " ".join(str(elem) for elem in self.elements)
        if self.semantic_action:
            result += f" / {self.semantic_action}"
        return result

# Правило грамматики
@dataclass
class GrammarRule:
    left_side: str
    alternatives: list[RuleAlternative]
    
    def __str__(self):
        return f"{self.left_side} = {' | '.join(str(alt) for alt in self.alternatives)};"

# Полная спецификация
@dataclass
class Specification:
    class_name: str
    tokens: list[str]
    type_mappings: list[TypeMapping]
    method_signatures: list[MethodSignature]
    grammar_rules: list[GrammarRule]
    axiom: str
    
    def __str__(self):
        result = []
        result.append(f"%class\n  {self.class_name}\n")
        result.append("%tokens\n  " + " ".join(self.tokens) + "\n")
        result.append("%types\n  " + "\n  ".join(str(tm) for tm in self.type_mappings) + "\n")
        result.append("%methods\n  " + "\n  ".join(str(ms) for ms in self.method_signatures) + "\n")
        result.append("%grammar\n  " + "\n  ".join(str(gr) for gr in self.grammar_rules) + "\n")
        result.append(f"%axiom\n  {self.axiom}\n")
        result.append("%end")
        return "\n".join(result)

# Определение лексической структуры и синтаксического анализатора

# Ключевые слова
KW_CLASS = pe.Terminal('KW_CLASS', '%class', lambda _: None, priority=10)
KW_TOKENS = pe.Terminal('KW_TOKENS', '%tokens', lambda _: None, priority=10)
KW_TYPES = pe.Terminal('KW_TYPES', '%types', lambda _: None, priority=10)
KW_METHODS = pe.Terminal('KW_METHODS', '%methods', lambda _: None, priority=10)
KW_GRAMMAR = pe.Terminal('KW_GRAMMAR', '%grammar', lambda _: None, priority=10)
KW_AXIOM = pe.Terminal('KW_AXIOM', '%axiom', lambda _: None, priority=10)
KW_END = pe.Terminal('KW_END', '%end', lambda _: None, priority=10)
KW_REP = pe.Terminal('KW_REP', '%rep', lambda _: None, priority=10)

# Идентификатор
ID = pe.Terminal('ID', '[A-Za-z][A-Za-z0-9_]*', str, priority=5)

# Знаки пунктуации
COLON = pe.Terminal('COLON', ':', lambda _: None, priority=10)
SEMICOLON = pe.Terminal('SEMICOLON', ';', lambda _: None, priority=10)
LPAREN = pe.Terminal('LPAREN', '\\(', lambda _: None, priority=10)
RPAREN = pe.Terminal('RPAREN', '\\)', lambda _: None, priority=10)
LBRACKET = pe.Terminal('LBRACKET', '\\[', lambda _: None, priority=10)
RBRACKET = pe.Terminal('RBRACKET', '\\]', lambda _: None, priority=10)
COMMA = pe.Terminal('COMMA', ',', lambda _: None, priority=10)
PIPE = pe.Terminal('PIPE', '\\|', lambda _: None, priority=10)
ASSIGN = pe.Terminal('ASSIGN', '=', lambda _: None, priority=10)
SLASH = pe.Terminal('SLASH', '/', lambda _: None, priority=10)

# Строковый литерал (для комментариев - не используется в грамматике)
STRING = pe.Terminal('STRING', '"[^"]*"', lambda s: s[1:-1], priority=5)

# Нетерминалы
NSpec = pe.NonTerminal('NSpec')
NClassSection = pe.NonTerminal('NClassSection')
NTokensSection = pe.NonTerminal('NTokensSection')
NTypesSection = pe.NonTerminal('NTypesSection')
NMethodsSection = pe.NonTerminal('NMethodsSection')
NGrammarSection = pe.NonTerminal('NGrammarSection')
NAxiomSection = pe.NonTerminal('NAxiomSection')

NIdList = pe.NonTerminal('NIdList')
NTypeMappingList = pe.NonTerminal('NTypeMappingList')
NMethodSignatureList = pe.NonTerminal('NMethodSignatureList')
NGrammarRuleList = pe.NonTerminal('NGrammarRuleList')

NTypeMapping = pe.NonTerminal('NTypeMapping')
NTypeName = pe.NonTerminal('NTypeName')
NMethodSignature = pe.NonTerminal('NMethodSignature')
NParamListOpt = pe.NonTerminal('NParamListOpt')
NParamList = pe.NonTerminal('NParamList')
NParam = pe.NonTerminal('NParam')

NGrammarRule = pe.NonTerminal('NGrammarRule')
NAlternatives = pe.NonTerminal('NAlternatives')
NRuleAlternative = pe.NonTerminal('NRuleAlternative')
NRuleElementList = pe.NonTerminal('NRuleElementList')
NSemanticActionOpt = pe.NonTerminal('NSemanticActionOpt')
NRuleElement = pe.NonTerminal('NRuleElement')
NNestedRule = pe.NonTerminal('NNestedRule')
NCommaSeparatedIdList = pe.NonTerminal('NCommaSeparatedIdList')

# Правила грамматики
# Общая структура
NSpec |= NClassSection, NTokensSection, NTypesSection, NMethodsSection, NGrammarSection, NAxiomSection, KW_END, \
    lambda cls, tokens, types, methods, grammar, axiom, _: Specification(
        class_name=cls,
        tokens=tokens,
        type_mappings=types,
        method_signatures=methods,
        grammar_rules=grammar,
        axiom=axiom
    )

# Секции
# Секция класса
NClassSection |= KW_CLASS, ID, lambda kw, name: name

# Секция токенов - список идентификаторов, разделенных пробелами
NTokensSection |= KW_TOKENS, NIdList, lambda kw, tokens: tokens

# Список идентификаторов через пробелы/новые строки
NIdList |= ID, lambda id_: [id_]
NIdList |= NIdList, ID, lambda ids, id_: ids + [id_]

# Секция типов - список отображений типов
NTypesSection |= KW_TYPES, NTypeMappingList, lambda kw, types: types

# Список отображений типов
NTypeMappingList |= lambda: []
NTypeMappingList |= NTypeMappingList, NTypeMapping, lambda types, type_mapping: types + [type_mapping]

# Отображение типов: список идентификаторов, тип
NTypeMapping |= NCommaSeparatedIdList, COLON, NTypeName, SEMICOLON, \
    lambda ids, colon, type_name, semicolon: TypeMapping(
        identifiers=ids,
        type_name=type_name[0],
        is_array=type_name[1]
    )

# Список идентификаторов через запятую
NCommaSeparatedIdList |= ID, lambda id_: [id_]
NCommaSeparatedIdList |= NCommaSeparatedIdList, COMMA, ID, lambda ids, comma, id_: ids + [id_]

# Имя типа с опциональным массивом
NTypeName |= ID, lambda name: (name, False)
NTypeName |= ID, LBRACKET, RBRACKET, lambda name, lb, rb: (name, True)

# Секция методов
NMethodsSection |= KW_METHODS, NMethodSignatureList, lambda kw, methods: methods

# Список сигнатур методов
NMethodSignatureList |= lambda: []
NMethodSignatureList |= NMethodSignatureList, NMethodSignature, lambda methods, method: methods + [method]

# Сигнатура метода: тип_возврата имя(параметры);
NMethodSignature |= NTypeName, ID, LPAREN, NParamListOpt, RPAREN, SEMICOLON, \
    lambda ret_type, name, lp, params, rp, semicolon: MethodSignature(
        return_type=ret_type[0] + ('[]' if ret_type[1] else ''),
        name=name,
        parameters=params
    )

# Опциональный список параметров
NParamListOpt |= lambda: []
NParamListOpt |= NParamList, lambda params: params

# Список параметров через запятую
NParamList |= NParam, lambda param: [param]
NParamList |= NParamList, COMMA, NParam, lambda params, comma, param: params + [param]

# Параметр: тип + опциональный []
NParam |= NTypeName, lambda type_name: (type_name[0], type_name[1])

# Секция грамматики
NGrammarSection |= KW_GRAMMAR, NGrammarRuleList, lambda kw, rules: rules

# Список правил грамматики
NGrammarRuleList |= lambda: []
NGrammarRuleList |= NGrammarRuleList, NGrammarRule, lambda rules, rule: rules + [rule]

# Правило грамматики: нетерминал = альтернативы;
NGrammarRule |= ID, ASSIGN, NAlternatives, SEMICOLON, \
    lambda nt, assign, alternatives, semicolon: GrammarRule(
        left_side=nt,
        alternatives=alternatives
    )

# Список альтернатив через |
NAlternatives |= NRuleAlternative, lambda alt: [alt]
NAlternatives |= NAlternatives, PIPE, NRuleAlternative, lambda alts, pipe, alt: alts + [alt]

# Альтернатива правила: последовательность элементов + опциональное семантическое действие
NRuleAlternative |= NRuleElementList, NSemanticActionOpt, \
    lambda elements, action: RuleAlternative(
        elements=elements,
        semantic_action=action
    )

# Список элементов правила
NRuleElementList |= lambda: []
NRuleElementList |= NRuleElementList, NRuleElement, lambda elements, element: elements + [element]

# Опциональное семантическое действие
NSemanticActionOpt |= lambda: None
NSemanticActionOpt |= SLASH, ID, lambda slash, action: action

# Элемент правила
NRuleElement |= ID, lambda id_: Terminal(id_) if id_.isupper() else NonTerminal(id_)
NRuleElement |= KW_REP, NRuleElement, lambda kw, element: Repetition(element)
NRuleElement |= LPAREN, NAlternatives, RPAREN, lambda lp, alts, rp: NestedRule(alts)

# Секция аксиомы
NAxiomSection |= KW_AXIOM, ID, SEMICOLON, lambda kw, axiom, semicolon: axiom

# Создание парсера
parser = pe.Parser(NSpec)

# Добавление пропускаемых доменов (пробелы и комментарии)
parser.add_skipped_domain('\\s+')  # Пробелы, табы, переводы строк
parser.add_skipped_domain('\\$.*')  # Комментарии от $ до конца строки

def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        try:
            with open(filename, 'r') as file:
                text = file.read()
                try:
                    spec = parser.parse_earley(text)
                    print(f"Разбор файла {filename} успешно завершен.")
                    print("Результат разбора:")
                    print(spec)
                except pe.Error as e:
                    print(f"Ошибка при разборе файла {filename}: {e}")
        except FileNotFoundError:
            print(f"Файл {filename} не найден.")
    else:
        # Если имя файла не указано, используем тестовый файл
        try:
            with open('test_spec.txt', 'r') as file:
                text = file.read()
                try:
                    spec = parser.parse_earley(text)
                    print("Разбор тестового файла успешно завершен.")
                    print("Результат разбора:")
                    print(spec)
                except pe.Error as e:
                    print(f"Ошибка при разборе тестового файла: {e}")
        except FileNotFoundError:
            print("Тестовый файл test_spec.txt не найден.")

if __name__ == "__main__":
    main() 