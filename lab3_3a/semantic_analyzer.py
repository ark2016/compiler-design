# Этот файл будет содержать логику семантического анализатора. 

import sys
import os
from dataclasses import dataclass
from typing import Dict, List, Set, Optional

sys.path.append(os.path.join(os.path.dirname(__file__), 'parser_edsl_v2'))
import parser_edsl as pe

from parser import Spec, TypeName, TypeMapping, ParamType, MethodSignature, GrammarRule, RuleAlternative, RuleElement, IdRuleElement, RepRuleElement, ParenRuleElement

# Классы для представления семантических ошибок
class SemanticError(pe.Error):
    def __init__(self, pos, message):
        self.pos = pos
        self.__message = message

    @property
    def message(self):
        return self.__message

class TokenInGrammarError(SemanticError):
    def __init__(self, pos, token_name):
        super().__init__(pos, f"Для терминального символа '{token_name}' не должно быть правил в секции %grammar")

class MissingRuleError(SemanticError):
    def __init__(self, pos, nonterm_name):
        super().__init__(pos, f"Для нетерминального символа '{nonterm_name}' должно быть правило в секции %grammar")

class DuplicateTypeError(SemanticError):
    def __init__(self, pos, symbol_name):
        super().__init__(pos, f"Символ '{symbol_name}' повторно определен в секции %types")

class MethodTypeMismatchError(SemanticError):
    def __init__(self, pos, method_name, alt_elements, param_types):
        super().__init__(pos, f"Типы в альтернативе не соответствуют параметрам метода '{method_name}': {alt_elements} != {param_types}")

class ActionNotDefinedError(SemanticError):
    def __init__(self, pos, action_name):
        super().__init__(pos, f"Метод '{action_name}' вызывается в правиле, но не определён в секции %methods")

class UnusedMethodError(SemanticError):
    def __init__(self, pos, method_name):
        super().__init__(pos, f"Метод '{method_name}' определен в секции %methods, но не используется в правилах")

class AlternativeTypeMismatchError(SemanticError):
    def __init__(self, pos, rule_name, type1, type2):
        super().__init__(pos, f"Несоответствие типов альтернатив в правиле '{rule_name}': {type1} != {type2}")

class ArrayTypeForRepError(SemanticError):
    def __init__(self, pos, element_type):
        super().__init__(pos, f"Тип элемента с префиксом %rep должен быть массивом, а не {element_type}")

# Класс для хранения типа символа
@dataclass
class SymbolType:
    name: str
    is_array: bool = False
    
    def __str__(self):
        return f"{self.name}{'[]' if self.is_array else ''}"

# Класс для анализа семантики
class SemanticAnalyzer:
    def __init__(self, spec: Spec):
        self.spec = spec
        self.defined_tokens = set()
        self.defined_types = {}  # name -> SymbolType
        self.defined_methods = {}  # name -> MethodSignature
        self.defined_rules = {}  # name -> GrammarRule
        self.used_methods = set()  # имена используемых методов
        
        self.initialize_symbols()
    
    def initialize_symbols(self):
        # Заполняем список токенов
        for token in self.spec.tokens:
            self.defined_tokens.add(token)
        
        # Заполняем словарь типов
        for type_mapping in self.spec.types:
            for name in type_mapping.names:
                self.defined_types[name] = SymbolType(
                    type_mapping.type.name,
                    type_mapping.type.is_array
                )
        
        # Заполняем словарь методов
        for method in self.spec.methods:
            self.defined_methods[method.name] = method
        
        # Заполняем словарь правил
        for rule in self.spec.grammar:
            self.defined_rules[rule.name] = rule
    
    def check(self):
        """Выполняет полную семантическую проверку спецификации"""
        # Проверка 1: Символы в секции %tokens не должны иметь правил в секции %grammar
        self.check_tokens_not_in_grammar()
        
        # Проверка 2: Все нетерминальные символы должны иметь правила в секции %grammar
        self.check_nonterminals_have_rules()
        
        # Проверка 3: Символ в секции %types должен встречаться не более одного раза
        self.check_type_uniqueness()
        
        # Проверка 4: Проверка типизации альтернатив
        self.check_alternative_typing()
        
        # Проверка 5: Все альтернативы в правиле должны иметь одинаковый тип
        self.check_alternatives_same_type()
        
        # Проверка 6: Тип элемента с префиксом %rep должен быть массивом
        self.check_rep_elements_array_type()
        
        # Проверка 7: Все методы, вызываемые из правил, должны быть определены в секции %methods
        # Проверка 8: Все определённые методы должны вызываться в правилах
        self.check_methods_usage()
    
    def check_tokens_not_in_grammar(self):
        """Проверка, что токены не имеют правил в секции грамматики"""
        for token in self.defined_tokens:
            if token in self.defined_rules:
                rule = self.defined_rules[token]
                raise TokenInGrammarError(rule.name_coord, token)
    
    def check_nonterminals_have_rules(self):
        """Проверка, что все нетерминальные символы имеют правила в грамматике"""
        # Собираем все символы, которые используются в правилах грамматики
        used_symbols = set()
        for rule_name, rule in self.defined_rules.items():
            used_symbols.add(rule_name)
            for alt in rule.alternatives:
                for elem in alt.elements:
                    if isinstance(elem, IdRuleElement):
                        used_symbols.add(elem.name)
                    elif isinstance(elem, ParenRuleElement):
                        # Внутри скобок все уже будет учтено при рекурсивном обходе
                        pass
        
        # Проверяем нетерминальные символы (которые не токены и не аксиома) на наличие правил
        for symbol in used_symbols:
            if symbol not in self.defined_tokens and symbol != self.spec.axiom and symbol not in self.defined_rules:
                # Ищем координату первого использования символа
                pos = None
                for rule in self.spec.grammar:
                    for alt in rule.alternatives:
                        for elem in alt.elements:
                            if isinstance(elem, IdRuleElement) and elem.name == symbol:
                                pos = elem.coord
                                break
                        if pos:
                            break
                    if pos:
                        break
                
                if not pos:  # Если не нашли, используем координату аксиомы
                    pos = self.spec.axiom_coord
                
                raise MissingRuleError(pos, symbol)
    
    def check_type_uniqueness(self):
        """Проверка уникальности типов в секции %types"""
        seen_types = set()
        for type_mapping in self.spec.types:
            for i, name in enumerate(type_mapping.names):
                if name in seen_types:
                    raise DuplicateTypeError(type_mapping.names_coords[i], name)
                seen_types.add(name)
    
    def check_alternative_typing(self):
        """Проверка типизации альтернатив в правилах"""
        for rule_name, rule in self.defined_rules.items():
            for alt in rule.alternatives:
                if alt.action:
                    # Альтернатива с вызовом метода
                    self.used_methods.add(alt.action)
                    
                    if alt.action not in self.defined_methods:
                        raise ActionNotDefinedError(alt.action_coord, alt.action)
                    
                    # Проверка соответствия типов параметров метода и элементов альтернативы
                    method = self.defined_methods[alt.action]
                    param_count = len(method.parameters)
                    typed_elements_count = self.count_typed_elements(alt.elements)
                    
                    if param_count != typed_elements_count:
                        raise MethodTypeMismatchError(
                            alt.action_coord,
                            alt.action,
                            f"{typed_elements_count} типизированных элементов",
                            f"{param_count} параметров"
                        )
    
    def count_typed_elements(self, elements):
        """Подсчет количества типизированных элементов в списке"""
        count = 0
        for elem in elements:
            if isinstance(elem, IdRuleElement):
                if elem.name in self.defined_types:
                    count += 1
            elif isinstance(elem, RepRuleElement):
                count += self.count_typed_elements([elem.element])
        return count
    
    def check_alternatives_same_type(self):
        """Проверка, что все альтернативы в правиле имеют одинаковый тип"""
        for rule_name, rule in self.defined_rules.items():
            if not rule.alternatives:
                continue
                
            # Определяем тип первой альтернативы
            first_alt_type = self.get_alternative_type(rule.alternatives[0])
            
            # Проверяем все остальные альтернативы
            for alt in rule.alternatives[1:]:
                alt_type = self.get_alternative_type(alt)
                
                # Если обе альтернативы нетипизированы, это нормально
                if first_alt_type is None and alt_type is None:
                    continue
                
                # Если типы не совпадают, это ошибка
                if first_alt_type != alt_type:
                    pos = alt.action_coord if alt.action_coord else rule.name_coord
                    raise AlternativeTypeMismatchError(
                        pos,
                        rule_name,
                        str(first_alt_type) if first_alt_type else "нетипизированная",
                        str(alt_type) if alt_type else "нетипизированная"
                    )
    
    def get_alternative_type(self, alt):
        """Определяет тип альтернативы"""
        if alt.action:
            # Если есть действие, тип определяется возвращаемым значением метода
            if alt.action in self.defined_methods:
                method = self.defined_methods[alt.action]
                return SymbolType(method.return_type.name, method.return_type.is_array)
            return None  # Метод не найден, но эта ошибка обрабатывается в другом месте
        
        # Если нет действия, то тип определяется единственным типизированным элементом
        typed_elements = []
        for elem in alt.elements:
            if isinstance(elem, IdRuleElement) and elem.name in self.defined_types:
                typed_elements.append(self.defined_types[elem.name])
        
        if len(typed_elements) == 1:
            return typed_elements[0]
        
        return None  # Нетипизированная альтернатива или несколько типизированных элементов
    
    def check_rep_elements_array_type(self):
        """Проверка, что элементы с префиксом %rep имеют тип массива"""
        for rule in self.spec.grammar:
            for alt in rule.alternatives:
                self.check_elements_rep_type(alt.elements)
    
    def check_elements_rep_type(self, elements):
        """Рекурсивная проверка типов элементов с %rep"""
        for elem in elements:
            if isinstance(elem, RepRuleElement):
                inner_elem = elem.element
                if isinstance(inner_elem, IdRuleElement) and inner_elem.name in self.defined_types:
                    elem_type = self.defined_types[inner_elem.name]
                    if not elem_type.is_array:
                        raise ArrayTypeForRepError(elem.rep_coord, str(elem_type))
                # Рекурсивно проверяем вложенные элементы
                if isinstance(inner_elem, ParenRuleElement):
                    for alt in inner_elem.alternatives:
                        self.check_elements_rep_type(alt.elements)
            elif isinstance(elem, ParenRuleElement):
                for alt in elem.alternatives:
                    self.check_elements_rep_type(alt.elements)
    
    def check_methods_usage(self):
        """Проверка использования методов"""
        # Проверяем, что все методы, используемые в правилах, определены
        for method_name in self.used_methods:
            if method_name not in self.defined_methods:
                # Найдем координату использования метода
                pos = None
                for rule in self.spec.grammar:
                    for alt in rule.alternatives:
                        if alt.action == method_name:
                            pos = alt.action_coord
                            break
                    if pos:
                        break
                
                raise ActionNotDefinedError(pos, method_name)
        
        # Проверяем, что все определенные методы используются
        for method_name, method in self.defined_methods.items():
            if method_name not in self.used_methods:
                raise UnusedMethodError(method.name_coord, method_name)

def analyze(spec: Spec):
    """Выполняет семантический анализ спецификации"""
    analyzer = SemanticAnalyzer(spec)
    analyzer.check()
    return True  # Если нет исключений, то анализ успешен 