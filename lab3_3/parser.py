import abc
import enum
import typing
from dataclasses import dataclass
from pprint import pprint

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), 'parser_edsl_v2'))
import parser_edsl as pe 
import re

# TypeName -> ID | ID LBRACKET RBRACKET
@dataclass
class TypeName:
    name: str
    name_coord: pe.Position
    is_array: bool = False

    def __str__(self):
        return f"{self.name}{'[]' if self.is_array else ''}"
    
    @staticmethod
    @pe.ExAction
    def create_simple(attrs, coords, res_coord):
        name, = attrs
        cname, = coords
        return TypeName(name, cname.start)
    
    @staticmethod
    @pe.ExAction
    def create_array(attrs, coords, res_coord):
        name, = attrs
        cname, clbracket, crbracket = coords
        return TypeName(name, cname.start, True)

# TypeMapping -> identifier_list : TypeName ;
@dataclass
class TypeMapping:
    names: list[str]
    names_coords: list[pe.Position]
    type: TypeName
    colon_coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        names_with_coords, type_name = attrs
        names, names_coords = names_with_coords
        cnames, ccolon, ctype, csemicolon = coords
        return TypeMapping(names, names_coords, type_name, ccolon.start)

# ParamType -> TypeName
@dataclass
class ParamType:
    type_name: TypeName

# MethodSignature -> TypeName identifier ( [ParamType_list] ) ;
@dataclass
class MethodSignature:
    return_type: TypeName
    name: str
    name_coord: pe.Position
    parameters: list[ParamType]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        return_type, name, params = attrs
        creturn_type, cname, clparen, cparams, crparen, csemicolon = coords
        return MethodSignature(return_type, name, cname.start, params)

# RuleElement (base class)
@dataclass
class RuleElement:
    pass

# RuleElement -> ID
@dataclass
class IdRuleElement(RuleElement):
    name: str
    coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        name, = attrs
        cname, = coords
        return IdRuleElement(name, cname.start)

# RuleElement -> %rep RuleElement
@dataclass
class RepRuleElement(RuleElement):
    element: RuleElement
    rep_coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        element, = attrs
        crep, celement = coords
        return RepRuleElement(element, crep.start)

# RuleElement -> ( Alternatives )
@dataclass
class ParenRuleElement(RuleElement):
    alternatives: list['RuleAlternative']
    lparen_coord: pe.Position
    rparen_coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        alternatives, = attrs
        clparen, calternatives, crparen = coords
        return ParenRuleElement(alternatives, clparen.start, crparen.start)

# RuleAlternative -> RuleElement+ [ / identifier ] | / identifier
@dataclass
class RuleAlternative:
    elements: list[RuleElement]
    action: str = None
    action_coord: pe.Position = None

    @staticmethod
    @pe.ExAction
    def create_with_elements_and_action(attrs, coords, res_coord):
        elements, action = attrs
        celements, cslash, caction = coords
        return RuleAlternative(elements, action, cslash.start)
    
    @staticmethod
    @pe.ExAction
    def create_with_elements(attrs, coords, res_coord):
        elements, = attrs
        celements, = coords
        return RuleAlternative(elements)
    
    @staticmethod
    @pe.ExAction
    def create_with_action(attrs, coords, res_coord):
        action, = attrs
        cslash, caction = coords
        return RuleAlternative([], action, cslash.start)

# GrammarRule -> identifier = Alternatives ;
@dataclass
class GrammarRule:
    name: str
    name_coord: pe.Position
    alternatives: list[RuleAlternative]

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        name, alternatives = attrs
        cname, cassign, calternatives, csemicolon = coords
        return GrammarRule(name, cname.start, alternatives)

# Spec -> ClassSection TokensSection TypesSection MethodsSection GrammarSection AxiomSection %end
@dataclass
class Spec:
    class_name: str
    class_name_coord: pe.Position
    tokens: list[str]
    tokens_coords: list[pe.Position]
    types: list[TypeMapping]
    methods: list[MethodSignature]
    grammar: list[GrammarRule]
    axiom: str
    axiom_coord: pe.Position

    @staticmethod
    @pe.ExAction
    def create(attrs, coords, res_coord):
        cls_info, tokens_info, types, methods, grammar, axiom_info = attrs
        cls_name, cls_coord = cls_info
        tokens, tokens_coords = tokens_info
        axiom_name, axiom_coord = axiom_info
        return Spec(cls_name, cls_coord, tokens, tokens_coords, types, methods, grammar, axiom_name, axiom_coord)

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
NSpec |= NClassSection, NTokensSection, NTypesSection, NMethodsSection, NGrammarSection, NAxiomSection, KW_END, Spec.create

@pe.ExAction
def create_class_section(attrs, coords, res_coord):
    name, = attrs
    ckw_class, cname = coords
    return (name, cname.start)

NClassSection |= KW_CLASS, ID, create_class_section

@pe.ExAction
def create_tokens_section(attrs, coords, res_coord):
    tokens = attrs[0]
    ckw_tokens, ctokens_fragment = coords
    tokens_coords = []
    for token in tokens:
        tokens_coords.append(ckw_tokens.start)
    return (tokens, tokens_coords)

NTokensSection |= KW_TOKENS, NSpaceSeparatedIdList_NonEmpty, create_tokens_section

# --- Секции с возможно пустыми списками ---
NTypesSection |= KW_TYPES, NTypeMapping_List, lambda mappings: mappings
NTypeMapping_List |= NTypeMapping, NTypeMapping_List, lambda h, t: [h] + t
NTypeMapping_List |= lambda: [] 

NMethodsSection |= KW_METHODS, NMethodSignature_List, lambda sigs: sigs
NMethodSignature_List |= NMethodSignature, NMethodSignature_List, lambda h, t: [h] + t
NMethodSignature_List |= lambda: [] 

NGrammarSection |= KW_GRAMMAR, NGrammarRule_List, lambda rules_list: rules_list
NGrammarRule_List |= NGrammarRule, NGrammarRule_List, lambda h, t: [h] + t
NGrammarRule_List |= lambda: []

@pe.ExAction
def create_axiom_section(attrs, coords, res_coord):
    axiom, = attrs
    ckw_axiom, caxiom, csemicolon = coords
    return (axiom, caxiom.start)

NAxiomSection |= KW_AXIOM, ID, SEMICOLON, create_axiom_section

# --- Определения списков и элементов (в основном право-рекурсивные) ---

# NSpaceSeparatedIdList_NonEmpty = ID+
NSpaceSeparatedIdList_NonEmpty |= ID, lambda id_val: [id_val]
NSpaceSeparatedIdList_NonEmpty |= ID, NSpaceSeparatedIdList_NonEmpty, lambda id_val, rest: [id_val] + rest 

@pe.ExAction
def create_comma_id_list(attrs, coords, res_coord):
    id_val, rest_ids = attrs
    cid, crest = coords
    coords_list = [cid.start] + [coord for coord in rest_ids[1] if coord is not None]
    return ([id_val] + rest_ids[0], coords_list)

# NCommaSeparatedIdList_NonEmpty = ID (COMMA ID)*
NCommaSeparatedIdList_NonEmpty |= ID, N_CommaId_Rest, create_comma_id_list

@pe.ExAction
def create_comma_id_rest(attrs, coords, res_coord):
    id_val, rest = attrs
    ccomma, cid, crest = coords
    rest_ids, rest_coords = rest if rest else ([], [])
    return ([id_val] + rest_ids, [cid.start] + rest_coords)

N_CommaId_Rest |= COMMA, ID, N_CommaId_Rest, create_comma_id_rest
N_CommaId_Rest |= lambda: ([], [])

# NTypeMapping = NCommaSeparatedIdList_NonEmpty COLON NTypeName SEMICOLON;
NTypeMapping |= NCommaSeparatedIdList_NonEmpty, COLON, NTypeName, SEMICOLON, TypeMapping.create

# NTypeName = ID (LBRACKET RBRACKET)?;
NTypeName |= ID, TypeName.create_simple
NTypeName |= ID, LBRACKET, RBRACKET, TypeName.create_array

# NMethodSignature = NTypeName ID LPAREN NParamList? RPAREN SEMICOLON;
NMethodSignature |= NTypeName, ID, LPAREN, NParamList, RPAREN, SEMICOLON, MethodSignature.create

# NParamList = NTypeName (COMMA NTypeName)*; 
NParamList |= NTypeName, N_CommaTypeName_Rest, lambda type_name, rest_params: [ParamType(type_name)] + rest_params
NParamList |= lambda: [] 

N_CommaTypeName_Rest |= COMMA, NTypeName, N_CommaTypeName_Rest, lambda type_name, rest: [ParamType(type_name)] + rest 
N_CommaTypeName_Rest |= lambda: []

# NGrammarRule = ID ASSIGN NAlternatives_NonEmpty SEMICOLON;
NGrammarRule |= ID, ASSIGN, NAlternatives_NonEmpty, SEMICOLON, GrammarRule.create

# NAlternatives_NonEmpty = NRuleAlternative (PIPE NRuleAlternative)*;
NAlternatives_NonEmpty |= NRuleAlternative, N_PipeAlternative_Rest, lambda alt, rest_alts: [alt] + rest_alts

N_PipeAlternative_Rest |= PIPE, NRuleAlternative, N_PipeAlternative_Rest, lambda alt, rest: [alt] + rest 
N_PipeAlternative_Rest |= lambda: [] 

# NRuleElement_List = NRuleElement* (используется для альтернатив)
NRuleElement_List |= NRuleElement, NRuleElement_List, lambda h, t: [h] + t
NRuleElement_List |= lambda: [] # Пустой список элементов

# NRuleElement_PlusList для NRuleElement+
NRuleElement_PlusList |= NRuleElement, NRuleElement_List, lambda h, t: [h] + t # Один или более элементов

# NRuleAlternative -> NRuleElement_NonEmptyList                       
#                  | NRuleElement_NonEmptyList SLASH ID              
#                  | SLASH ID                                        
NRuleAlternative |= NRuleElement_PlusList, SLASH, ID, RuleAlternative.create_with_elements_and_action
NRuleAlternative |= NRuleElement_PlusList, RuleAlternative.create_with_elements
NRuleAlternative |= SLASH, ID, RuleAlternative.create_with_action

# NRuleElement = ID | LPAREN NAlternatives_NonEmpty RPAREN | KW_REP NRuleElement;
NRuleElement |= ID, IdRuleElement.create
NRuleElement |= LPAREN, NAlternatives_NonEmpty, RPAREN, ParenRuleElement.create
NRuleElement |= KW_REP, NRuleElement, RepRuleElement.create

# Создание парсера
parser = pe.Parser(NSpec)
parser.add_skipped_domain('\\s')  # игнорируем пробелы и переносы строк
parser.add_skipped_domain('\\$[^\n]*')  # игнорируем комментарии, начинающиеся с $
parser.add_skipped_domain('//[^\n]*')  # игнорируем комментарии в стиле C++

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