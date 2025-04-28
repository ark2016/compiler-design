import parser_edsl as pe
import re
from dataclasses import dataclass
from typing import List, Optional, Union
from pprint import pprint

# =============================================================================
# 1. Определение AST для спецификации генератора парсеров через data‑классы
# =============================================================================

@dataclass
class Spec:
    class_name: str
    tokens: List[str]
    types: List['TypeMapping']
    methods: List['MethodSignature']
    grammar: List['GrammarRule']
    axiom: str

@dataclass
class TypeMapping:
    ids: List[str]
    type_name: str

@dataclass
class MethodSignature:
    ret_type: str
    name: str
    params: List[str]

@dataclass
class GrammarRule:
    nonterm: str
    alternatives: List['RuleAlternative']

@dataclass
class RuleAlternative:
    elements: List[Union[str, 'GroupedAlternative', 'Repetition']]
    action: Optional[str] = None

@dataclass
class GroupedAlternative:
    alternatives: List['RuleAlternative']

@dataclass
class Repetition:
    element: Union[str, 'GroupedAlternative', 'Repetition']

# =============================================================================
# 2. Грамматика спецификации 
# =============================================================================

# Терминальные символы для спецификации
KW_CLASS   = pe.Terminal("KW_CLASS",   r"%class",    lambda s: s)
KW_TOKENS  = pe.Terminal("KW_TOKENS",  r"%tokens",   lambda s: s)
KW_TYPES   = pe.Terminal("KW_TYPES",   r"%types",    lambda s: s)
KW_METHODS = pe.Terminal("KW_METHODS", r"%methods",  lambda s: s)
KW_GRAMMAR = pe.Terminal("KW_GRAMMAR", r"%grammar",  lambda s: s)
KW_AXIOM   = pe.Terminal("KW_AXIOM",   r"%axiom",    lambda s: s)
KW_END     = pe.Terminal("KW_END",     r"%end",      lambda s: s)
KW_REP     = pe.Terminal("KW_REP",     r"%rep",      lambda s: s)

# Знаки пунктуации
COLON      = pe.Terminal("COLON",      r":",         lambda s: s)
SEMICOLON  = pe.Terminal("SEMICOLON",  r";",         lambda s: s)
LPAREN     = pe.Terminal("LPAREN",     r"\(",        lambda s: s)
RPAREN     = pe.Terminal("RPAREN",     r"\)",        lambda s: s)
LBRACKET   = pe.Terminal("LBRACKET",   r"\[",        lambda s: s)
RBRACKET   = pe.Terminal("RBRACKET",   r"\]",        lambda s: s)
COMMA      = pe.Terminal("COMMA",      r",",         lambda s: s)
SLASH      = pe.Terminal("SLASH",      r"/",         lambda s: s)
PIPE       = pe.Terminal("PIPE",       r"\|",        lambda s: s)
ASSIGN     = pe.Terminal("ASSIGN",     r"=",         lambda s: s)

# Идентификатор
ID         = pe.Terminal("ID",         r"[A-Za-z][A-Za-z0-9_]*", lambda s: s)

# Нетерминальные символы спецификации
NSpec                 = pe.NonTerminal("NSpec")
NClassSection         = pe.NonTerminal("NClassSection")
NTokensSection        = pe.NonTerminal("NTokensSection")
NTypesSection         = pe.NonTerminal("NTypesSection")
NMethodsSection       = pe.NonTerminal("NMethodsSection")
NGrammarSection       = pe.NonTerminal("NGrammarSection")
NAxiomSection         = pe.NonTerminal("NAxiomSection")
NSpaceSeparatedIdList = pe.NonTerminal("NSpaceSeparatedIdList")
NCommaSeparatedIdList = pe.NonTerminal("NCommaSeparatedIdList")
NTypeMapping          = pe.NonTerminal("NTypeMapping")
NTypeName             = pe.NonTerminal("NTypeName")
NMethodSignature      = pe.NonTerminal("NMethodSignature")
NParamList            = pe.NonTerminal("NParamList")
NGrammarRule          = pe.NonTerminal("NGrammarRule")
NAlternatives         = pe.NonTerminal("NAlternatives")
NRuleAlternative      = pe.NonTerminal("NRuleAlternative")
NRuleElement          = pe.NonTerminal("NRuleElement")
# Вспомогательный нетерминал для сбора элементов альтернативы
NRuleAltElements      = pe.NonTerminal("NRuleAltElements")

# Правила спецификации

# NSpec = NClassSection NTokensSection NTypesSection NMethodsSection NGrammarSection NAxiomSection KW_END
NSpec |= (NClassSection, NTokensSection, NTypesSection, NMethodsSection,
         NGrammarSection, NAxiomSection, KW_END,
         lambda cs, ts, tys, ms, gs, ax, end: Spec(
             class_name=cs,
             tokens=ts,
             types=tys,
             methods=ms,
             grammar=gs,
             axiom=ax
         ))

# NClassSection → KW_CLASS ID
NClassSection |= (KW_CLASS, ID, lambda kw, ident: ident)

# NTokensSection → KW_TOKENS NSpaceSeparatedIdList
NTokensSection |= (KW_TOKENS, NSpaceSeparatedIdList, lambda kw, id_list: id_list)
# NSpaceSeparatedIdList = ID+
NSpaceSeparatedIdList |= (ID, lambda id: [id])
NSpaceSeparatedIdList |= (NSpaceSeparatedIdList, ID, lambda lst, id: lst + [id])

# NTypesSection → KW_TYPES NTypeMapping*
NTypesSection |= (KW_TYPES, lambda kw: [])
NTypesSection |= (NTypesSection, NTypeMapping, lambda lst, mapping: lst + [mapping])

# NTypeMapping → NCommaSeparatedIdList COLON NTypeName SEMICOLON
NTypeMapping |= (NCommaSeparatedIdList, COLON, NTypeName, SEMICOLON,
                lambda ids, colon, typename, sem: TypeMapping(ids=ids, type_name=typename))
# NCommaSeparatedIdList = ID (COMMA ID)*
NCommaSeparatedIdList |= (ID, lambda id: [id])
NCommaSeparatedIdList |= (NCommaSeparatedIdList, COMMA, ID, lambda lst, comma, id: lst + [id])
# NTypeName → ID (LBRACKET RBRACKET)?
NTypeName |= (ID, lambda id: id)
NTypeName |= (ID, LBRACKET, RBRACKET, lambda id, lb, rb: id)

# NMethodsSection → KW_METHODS NMethodSignature*
NMethodsSection |= (KW_METHODS, lambda kw: [])
NMethodsSection |= (NMethodsSection, NMethodSignature, lambda lst, sig: lst + [sig])
# NMethodSignature → NTypeName ID LPAREN NParamList? RPAREN SEMICOLON
NMethodSignature |= (NTypeName, ID, LPAREN, RPAREN, SEMICOLON,
                    lambda typename, id, lp, rp, sem: MethodSignature(ret_type=typename, name=id, params=[]))
NMethodSignature |= (NTypeName, ID, LPAREN, NParamList, RPAREN, SEMICOLON,
                    lambda typename, id, lp, params, rp, sem: MethodSignature(ret_type=typename, name=id, params=params))
# NParamList = NTypeName (COMMA NTypeName)*
NParamList |= (NTypeName, lambda typename: [typename])
NParamList |= (NParamList, COMMA, NTypeName, lambda lst, comma, typename: lst + [typename])

# NGrammarSection → KW_GRAMMAR NGrammarRule*
NGrammarSection |= (KW_GRAMMAR, lambda kw: [])
NGrammarSection |= (NGrammarSection, NGrammarRule, lambda lst, rule: lst + [rule])
# NGrammarRule → ID ASSIGN NAlternatives SEMICOLON
NGrammarRule |= (ID, ASSIGN, NAlternatives, SEMICOLON,
                 lambda id, assign, alts, sem: GrammarRule(nonterm=id, alternatives=alts))
# NAlternatives → NRuleAlternative (PIPE NRuleAlternative)*
NAlternatives |= (NRuleAlternative, lambda alt: [alt])
NAlternatives |= (NAlternatives, PIPE, NRuleAlternative, lambda lst, pipe, alt: lst + [alt])
# NRuleAlternative → NRuleElement* (SLASH ID)?
NRuleAltElements |= (lambda: [])
NRuleAltElements |= (NRuleAltElements, NRuleElement, lambda lst, el: lst + [el])
NRuleAlternative |= (NRuleAltElements, lambda lst: RuleAlternative(elements=lst, action=None))
NRuleAlternative |= (NRuleAltElements, SLASH, ID, lambda lst, slash, act: RuleAlternative(elements=lst, action=act))
# NRuleElement → ID | LPAREN NAlternatives RPAREN | KW_REP NRuleElement
NRuleElement |= (ID, lambda id: id)
NRuleElement |= (LPAREN, NAlternatives, RPAREN, lambda lp, alts, rp: GroupedAlternative(alternatives=alts))
NRuleElement |= (KW_REP, NRuleElement, lambda rep, el: Repetition(element=el))

# NAxiomSection → KW_AXIOM ID SEMICOLON
NAxiomSection |= (KW_AXIOM, ID, SEMICOLON, lambda kw, id, sem: id)

# Создаем парсер спецификации с аксиомой NSpec
spec_parser = pe.Parser(NSpec)
spec_parser.add_skipped_domain(r"\s+")
spec_parser.add_skipped_domain(r"\$.*")

# =============================================================================
# Тестирование парсера спецификации
# =============================================================================

if __name__ == "__main__":
    # Пример входного файла спецификации
    spec_text = r"""
    %class MyParser
    %tokens ID NUMBER
    %types A: TypeA;
    %methods void dummy();
    %grammar
    Spec = A;
    %axiom Spec;
    %end
    """
    # ast = spec_parser.parse_earley(spec_text)
    try:
        ast = spec_parser.parse_earley(spec_text)
        print("AST спецификации:")
        pprint(ast)
    except Exception as e:
        print("Ошибка при разборе спецификации:", e)
