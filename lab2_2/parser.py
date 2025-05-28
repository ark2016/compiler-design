import abc
import enum
import typing
from dataclasses import dataclass

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), 'parser_edsl_v2'))
import parser_edsl as pe 
import re

# TypeName -> ID | ID LBRACKET RBRACKET
@dataclass
class TypeName:
    name: str
    is_array: bool = False

    def __str__(self):
        return f"{self.name}{'[]' if self.is_array else ''}"

# TypeMapping -> identifier_list : TypeName ;
@dataclass
class TypeMapping:
    names: list[str]
    type: TypeName

# ParamType -> TypeName
@dataclass
class ParamType:
    type_name: TypeName

# MethodSignature -> TypeName identifier ( [ParamType_list] ) ;
@dataclass
class MethodSignature:
    return_type: TypeName
    name: str
    parameters: list[ParamType]

# RuleElement (base class)
@dataclass
class RuleElement:
    pass

# RuleElement -> ID
@dataclass
class IdRuleElement(RuleElement):
    name: str

# RuleElement -> %rep RuleElement
@dataclass
class RepRuleElement(RuleElement):
    element: RuleElement

# RuleElement -> ( Alternatives )
@dataclass
class ParenRuleElement(RuleElement):
    alternatives: list['RuleAlternative'] # Forward declaration as string

# RuleAlternative -> RuleElement+ [ / identifier ] | / identifier
@dataclass
class RuleAlternative:
    elements: list[RuleElement]
    action: str = None

# GrammarRule -> identifier = Alternatives ;
@dataclass
class GrammarRule:
    name: str
    alternatives: list[RuleAlternative]

# Spec -> ClassSection TokensSection TypesSection MethodsSection GrammarSection AxiomSection %end
@dataclass
class Spec:
    class_name: str
    tokens: list[str]
    types: list[TypeMapping]
    methods: list[MethodSignature]
    grammar: list[GrammarRule]
    axiom: str

# Создание терминальных символов
ID = pe.Terminal('ID', '[A-Za-z][A-Za-z0-9_-]*', str)

# Ключевые слова
KW_CLASS = pe.Terminal('class', '%class', lambda x: None, priority=10)
KW_TOKENS = pe.Terminal('tokens', '%tokens', lambda x: None, priority=10)
KW_TYPES = pe.Terminal('types', '%types', lambda x: None, priority=10)
KW_METHODS = pe.Terminal('methods', '%methods', lambda x: None, priority=10)
KW_GRAMMAR = pe.Terminal('grammar', '%grammar', lambda x: None, priority=10)
KW_AXIOM = pe.Terminal('axiom', '%axiom', lambda x: None, priority=10)
KW_END = pe.Terminal('end', '%end', lambda x: None, priority=10)
KW_REP = pe.Terminal('rep', '%rep', lambda x: None, priority=10)

# Разделители и операторы
COMMA = pe.Terminal('comma', ',', lambda x: None, priority=10)
COLON = pe.Terminal('colon', ':', lambda x: None, priority=10)
SEMICOLON = pe.Terminal('semicolon', ';', lambda x: None, priority=10)
LPAREN = pe.Terminal('lparen', '\\(', lambda x: None, priority=10)
RPAREN = pe.Terminal('rparen', '\\)', lambda x: None, priority=10)
LBRACKET = pe.Terminal('lbracket', '\\[', lambda x: None, priority=10)
RBRACKET = pe.Terminal('rbracket', '\\]', lambda x: None, priority=10)
ASSIGN = pe.Terminal('assign', '=', lambda x: None, priority=10)
PIPE = pe.Terminal('pipe', '\\|', lambda x: None, priority=10)
SLASH = pe.Terminal('slash', '/', lambda x: None, priority=10)

# ========================================================================
# ОПРЕДЕЛЕНИЕ ВСЕХ НЕТЕРМИНАЛЬНЫХ СИМВОЛОВ ПЕРЕД ОПРЕДЕЛЕНИЕМ ПРАВИЛ
# ========================================================================

# Основные секции
NSpec = pe.NonTerminal('NSpec')
NClassSection = pe.NonTerminal('NClassSection')
NTokensSection = pe.NonTerminal('NTokensSection')
NTypesSection = pe.NonTerminal('NTypesSection')
NMethodsSection = pe.NonTerminal('NMethodsSection')
NGrammarSection = pe.NonTerminal('NGrammarSection')
NAxiomSection = pe.NonTerminal('NAxiomSection')

# Элементы структуры
NTypeName = pe.NonTerminal('NTypeName')
NTypeMapping = pe.NonTerminal('NTypeMapping')
NMethodSignature = pe.NonTerminal('NMethodSignature')
NGrammarRule = pe.NonTerminal('NGrammarRule')
NRuleAlternative = pe.NonTerminal('NRuleAlternative')
NRuleElement = pe.NonTerminal('NRuleElement')


# Вспомогательные нетерминалы для списков (могут быть пустыми)
NTypeMapping_List = pe.NonTerminal('NTypeMapping_List')
NMethodSignature_List = pe.NonTerminal('NMethodSignature_List')
NGrammarRule_List = pe.NonTerminal('NGrammarRule_List')
N_CommaId_Rest = pe.NonTerminal('N_CommaId_Rest')
NParamList = pe.NonTerminal('NParamList')
N_CommaTypeName_Rest = pe.NonTerminal('N_CommaTypeName_Rest')
N_PipeAlternative_Rest = pe.NonTerminal('N_PipeAlternative_Rest')
NRuleElement_List = pe.NonTerminal('NRuleElement_List')


# Вспомогательные нетерминалы для списков (НЕ могут быть пустыми)
NSpaceSeparatedIdList_NonEmpty = pe.NonTerminal('NSpaceSeparatedIdList_NonEmpty')
NCommaSeparatedIdList_NonEmpty = pe.NonTerminal('NCommaSeparatedIdList_NonEmpty')
NAlternatives_NonEmpty = pe.NonTerminal('NAlternatives_NonEmpty')
NRuleElement_PlusList = pe.NonTerminal('NRuleElement_PlusList')

# ========================================================================
# ОПРЕДЕЛЕНИЕ ПРАВИЛ ГРАММАТИКИ
# ========================================================================

# Правила грамматики
# KW_END attr (None) is filtered
NSpec |= NClassSection, NTokensSection, NTypesSection, NMethodsSection, NGrammarSection, NAxiomSection, KW_END, \
         lambda cls, tokens, types, methods, grammar, axiom: Spec(cls, tokens, types, methods, grammar, axiom)

# KW_CLASS attr (None) is filtered
NClassSection |= KW_CLASS, ID, lambda name: name

# KW_TOKENS attr (None) is filtered
NTokensSection |= KW_TOKENS, NSpaceSeparatedIdList_NonEmpty, lambda tokens: tokens

# --- Секции с возможно пустыми списками ---
# KW_TYPES attr (None) filtered
NTypesSection |= KW_TYPES, NTypeMapping_List, lambda mappings: mappings
NTypeMapping_List |= NTypeMapping, NTypeMapping_List, lambda h, t: [h] + t
NTypeMapping_List |= lambda: [] # Пустой список

# KW_METHODS attr (None) filtered
NMethodsSection |= KW_METHODS, NMethodSignature_List, lambda sigs: sigs
NMethodSignature_List |= NMethodSignature, NMethodSignature_List, lambda h, t: [h] + t
NMethodSignature_List |= lambda: [] # Пустой список

# KW_GRAMMAR attr (None) filtered
NGrammarSection |= KW_GRAMMAR, NGrammarRule_List, lambda rules_list: rules_list
NGrammarRule_List |= NGrammarRule, NGrammarRule_List, lambda h, t: [h] + t
NGrammarRule_List |= lambda: [] # Пустой список


# KW_AXIOM (None) and SEMICOLON (None) attrs are filtered
NAxiomSection |= KW_AXIOM, ID, SEMICOLON, lambda axiom: axiom


# --- Определения списков и элементов (в основном право-рекурсивные) ---

# NSpaceSeparatedIdList_NonEmpty = ID+
NSpaceSeparatedIdList_NonEmpty |= ID, lambda id_val: [id_val]
NSpaceSeparatedIdList_NonEmpty |= ID, NSpaceSeparatedIdList_NonEmpty, lambda id_val, rest: [id_val] + rest #Праворекурсивный для (ID ID+)

# NCommaSeparatedIdList_NonEmpty = ID (COMMA ID)*
NCommaSeparatedIdList_NonEmpty |= ID, N_CommaId_Rest, lambda id_val, rest_ids: [id_val] + rest_ids
N_CommaId_Rest |= COMMA, ID, N_CommaId_Rest, lambda id_val, rest: [id_val] + rest # COMMA (None) filtered
N_CommaId_Rest |= lambda: [] # Пустой хвост

# NTypeMapping = NCommaSeparatedIdList_NonEmpty COLON NTypeName SEMICOLON;
# COLON (None), SEMICOLON (None) attrs filtered
NTypeMapping |= NCommaSeparatedIdList_NonEmpty, COLON, NTypeName, SEMICOLON, \
                lambda names, type_name: TypeMapping(names, type_name)

# NTypeName = ID (LBRACKET RBRACKET)?;
NTypeName |= ID, lambda id_val: TypeName(id_val)
# LBRACKET (None), RBRACKET (None) attrs filtered
NTypeName |= ID, LBRACKET, RBRACKET, lambda id_val: TypeName(id_val, True)

# NMethodSignature = NTypeName ID LPAREN NParamList? RPAREN SEMICOLON;
# LPAREN (None), RPAREN (None), SEMICOLON (None) attrs filtered
NMethodSignature |= NTypeName, ID, LPAREN, NParamList, RPAREN, SEMICOLON, \
                    lambda return_type, name, params: MethodSignature(return_type, name, params)

# NParamList = NTypeName (COMMA NTypeName)*; (может быть пустым, если NParamList не было вообще)
NParamList |= NTypeName, N_CommaTypeName_Rest, lambda type_name, rest_params: [ParamType(type_name)] + rest_params
NParamList |= lambda: [] # Пустой список параметров

N_CommaTypeName_Rest |= COMMA, NTypeName, N_CommaTypeName_Rest, lambda type_name, rest: [ParamType(type_name)] + rest # COMMA (None) filtered
N_CommaTypeName_Rest |= lambda: [] # Пустой хвост

# NGrammarRule = ID ASSIGN NAlternatives_NonEmpty SEMICOLON;
# ASSIGN (None), SEMICOLON (None) attrs filtered
NGrammarRule |= ID, ASSIGN, NAlternatives_NonEmpty, SEMICOLON, \
                lambda name, alternatives: GrammarRule(name, alternatives)

# NAlternatives_NonEmpty = NRuleAlternative (PIPE NRuleAlternative)*;
NAlternatives_NonEmpty |= NRuleAlternative, N_PipeAlternative_Rest, lambda alt, rest_alts: [alt] + rest_alts

N_PipeAlternative_Rest |= PIPE, NRuleAlternative, N_PipeAlternative_Rest, lambda alt, rest: [alt] + rest # PIPE (None) filtered
N_PipeAlternative_Rest |= lambda: [] # Пустой хвост

# NRuleElement_List = NRuleElement* (используется для альтернатив)
NRuleElement_List |= NRuleElement, NRuleElement_List, lambda h, t: [h] + t
NRuleElement_List |= lambda: [] # Пустой список элементов

# NRuleElement_PlusList для NRuleElement+
NRuleElement_PlusList |= NRuleElement, NRuleElement_List, lambda h, t: [h] + t # Один или более элементов

# Пересмотренный NRuleAlternative на основе grammar.md и избегая неоднозначности с пустым NRuleElement_List
# NRuleAlternative -> NRuleElement_NonEmptyList                       // Elements, no action
#                  | NRuleElement_NonEmptyList SLASH ID              // Elements, with action
#                  | SLASH ID                                        // No elements, with action
NRuleAlternative |= NRuleElement_PlusList, SLASH, ID, lambda elements, action_id: RuleAlternative(elements, action_id) # SLASH (None) filtered
NRuleAlternative |= NRuleElement_PlusList, lambda elements: RuleAlternative(elements)
NRuleAlternative |= SLASH, ID, lambda action_id: RuleAlternative([], action_id) # SLASH (None) filtered


# NRuleElement = ID | LPAREN NAlternatives_NonEmpty RPAREN | KW_REP NRuleElement;
NRuleElement |= ID, lambda id_val: IdRuleElement(id_val)
# LPAREN (None), RPAREN (None) attrs filtered
NRuleElement |= LPAREN, NAlternatives_NonEmpty, RPAREN, \
                lambda alternatives: ParenRuleElement(alternatives)
# KW_REP attr (None) is filtered
NRuleElement |= KW_REP, NRuleElement, lambda elem: RepRuleElement(elem)


# Создание парсера
parser = pe.Parser(NSpec)
parser.add_skipped_domain('\\s')  # игнорируем пробелы и переносы строк
parser.add_skipped_domain('\\$[^\n]*') 
parser.add_skipped_domain('//[^\n]*') 

def parse_spec(text):
    """Разбор текста спецификации"""
    try:
        return parser.parse(text)
    except pe.Error as e:
        print(f"Ошибка разбора: {e.message}")
        print(f"  Позиция: {e.pos if hasattr(e, 'pos') else 'N/A'}")
        if hasattr(e, 'unexpected') and hasattr(e, 'expected'):
            print(f"  Неожиданный токен: {e.unexpected}")
            print(f"  Ожидались: {e.expected}")
        return None
    except Exception as e_gen:
        print(f"Общая ошибка при разборе: {e_gen}")
        import traceback
        traceback.print_exc()
        return None


if __name__ == "__main__":
    import sys
    
    if len(sys.argv) > 1:
        filepath = sys.argv[1]
        if not os.path.exists(filepath):
            print(f"Ошибка: Файл '{filepath}' не найден.")
        else:
            with open(filepath, 'r') as f:
                spec_text = f.read()
            print(f"--- Начало разбора файла: {filepath} ---")
            spec = parse_spec(spec_text)
            if spec:
                print("--- Разбор успешно завершен! ---")
                print("\nРезультаты разбора спецификации:")
                print("===============================")
                print(f"Имя класса: {spec.class_name}")
                
                print("\nТокены:")
                if spec.tokens:
                    for token in spec.tokens:
                        print(f"  {token}")
                else:
                    print("  (нет)")

                print("\nТипы:")
                if spec.types:
                    for type_mapping in spec.types:
                        names = ', '.join(type_mapping.names)
                        print(f"  {names}: {type_mapping.type}")
                else:
                    print("  (нет)")
                
                print("\nМетоды:")
                if spec.methods:
                    for method in spec.methods:
                        params = [str(param.type_name) for param in method.parameters]
                        param_str = ', '.join(params)
                        print(f"  {method.return_type} {method.name}({param_str})")
                else:
                    print("  (нет)")
                
                print("\nПравила грамматики:")
                if spec.grammar:
                    for rule in spec.grammar:
                        print(f"  {rule.name} =")
                        for i, alt in enumerate(rule.alternatives):
                            alt_str = []
                            for elem in alt.elements:
                                if isinstance(elem, IdRuleElement):
                                    alt_str.append(elem.name)
                                elif isinstance(elem, RepRuleElement):
                                    rep_elem_str = "?"
                                    if isinstance(elem.element, IdRuleElement):
                                        rep_elem_str = elem.element.name
                                    elif isinstance(elem.element, ParenRuleElement):
                                        rep_elem_str = "(...)" 
                                    alt_str.append(f"%rep {rep_elem_str}")
                                elif isinstance(elem, ParenRuleElement):
                                    alt_str.append("(...)") 
                            
                            rule_line = " ".join(alt_str)
                            if alt.action:
                                rule_line += f" / {alt.action}"
                            
                            prefix = "    | " if i > 0 else "      "
                            if not alt.elements and alt.action:
                                rule_line = f"/ {alt.action}" 
                            elif not alt.elements and not alt.action: # Это не должно происходить после исправлений
                                rule_line = "<пустая альтернатива - ошибка>" # Запасной вариант
                            
                            print(f"{prefix}{rule_line}") 
                        print()
                else:
                    print("  (нет)")
                
                print(f"Аксиома: {spec.axiom}")
                print("--- Конец результатов разбора ---")
            else:
                print("--- Разбор не удался. ---")
    else:
        print("Использование: python parser.py <файл_спецификации>")