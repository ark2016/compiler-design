#!/usr/bin/env python3
"""
Тесты для семантического анализатора языка спецификации грамматик.
Демонстрирует обнаружение различных семантических ошибок.
"""
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), 'parser_edsl_v2'))
import parser_edsl as pe

from parser import TypeName, TypeMapping, MethodSignature, ParamType, GrammarRule, RuleAlternative, IdRuleElement, RepRuleElement, Spec
from semantic_analyzer import (
    analyze, SemanticError, DuplicateTypeError, TokenInGrammarError, 
    UnusedMethodError, ActionNotDefinedError, ArrayTypeForRepError
)

def test_duplicate_type():
    """Тест обнаружения ошибки дублирования типа"""
    # Создаем позицию для координат
    pos = pe.Position(1, 1)
    
    # Создаем спецификацию с ошибкой - дублирование типа NUMBER
    spec = Spec(
        class_name="TestParser",
        class_name_coord=pos,
        tokens=["NUMBER", "PLUS"],
        tokens_coords=[pos, pos],
        types=[
            TypeMapping(names=["Expr"], names_coords=[pos], type=TypeName("ArithmExpr", pos), colon_coord=pos),
            TypeMapping(names=["NUMBER"], names_coords=[pos], type=TypeName("ArithmExpr", pos), colon_coord=pos),
            TypeMapping(names=["NUMBER"], names_coords=[pos], type=TypeName("Integer", pos), colon_coord=pos)  # Ошибка: дублирование
        ],
        methods=[],
        grammar=[
            GrammarRule(name="Expr", name_coord=pos, alternatives=[
                RuleAlternative(elements=[IdRuleElement("NUMBER", pos)])
            ])
        ],
        axiom="Expr",
        axiom_coord=pos
    )
    
    # Проверяем семантику
    try:
        analyze(spec)
        print("Ошибка! Семантический анализатор не обнаружил ошибку дублирования типа.")
        return False
    except DuplicateTypeError as e:
        print(f"Тест 'duplicate_type': Ошибка обнаружена - {e.message}")
        return True
    except SemanticError as e:
        print(f"Тест 'duplicate_type': Обнаружена другая ошибка - {e.message}")
        return False

def test_token_in_grammar():
    """Тест обнаружения ошибки правила для токена"""
    # Создаем позицию для координат
    pos = pe.Position(1, 1)
    
    # Создаем спецификацию с ошибкой - токен PLUS имеет правило в грамматике
    spec = Spec(
        class_name="TestParser",
        class_name_coord=pos,
        tokens=["NUMBER", "PLUS"],
        tokens_coords=[pos, pos],
        types=[
            TypeMapping(names=["Expr"], names_coords=[pos], type=TypeName("ArithmExpr", pos), colon_coord=pos),
        ],
        methods=[],
        grammar=[
            GrammarRule(name="Expr", name_coord=pos, alternatives=[
                RuleAlternative(elements=[IdRuleElement("NUMBER", pos)])
            ]),
            GrammarRule(name="PLUS", name_coord=pos, alternatives=[  # Ошибка: PLUS - это токен
                RuleAlternative(elements=[IdRuleElement("NUMBER", pos)])
            ])
        ],
        axiom="Expr",
        axiom_coord=pos
    )
    
    # Проверяем семантику
    try:
        analyze(spec)
        print("Ошибка! Семантический анализатор не обнаружил ошибку правила для токена.")
        return False
    except TokenInGrammarError as e:
        print(f"Тест 'token_in_grammar': Ошибка обнаружена - {e.message}")
        return True
    except SemanticError as e:
        print(f"Тест 'token_in_grammar': Обнаружена другая ошибка - {e.message}")
        return False

def test_unused_method():
    """Тест обнаружения ошибки неиспользуемого метода"""
    # Создаем позицию для координат
    pos = pe.Position(1, 1)
    
    # Создаем спецификацию с ошибкой - неиспользуемый метод
    spec = Spec(
        class_name="TestParser",
        class_name_coord=pos,
        tokens=["NUMBER", "PLUS"],
        tokens_coords=[pos, pos],
        types=[
            TypeMapping(names=["Expr"], names_coords=[pos], type=TypeName("ArithmExpr", pos), colon_coord=pos),
        ],
        methods=[
            MethodSignature(
                return_type=TypeName("ArithmExpr", pos),
                name="unused_method",
                name_coord=pos,
                parameters=[]
            )
        ],
        grammar=[
            GrammarRule(name="Expr", name_coord=pos, alternatives=[
                RuleAlternative(elements=[IdRuleElement("NUMBER", pos)])
            ])
        ],
        axiom="Expr",
        axiom_coord=pos
    )
    
    # Проверяем семантику
    try:
        analyze(spec)
        print("Ошибка! Семантический анализатор не обнаружил ошибку неиспользуемого метода.")
        return False
    except UnusedMethodError as e:
        print(f"Тест 'unused_method': Ошибка обнаружена - {e.message}")
        return True
    except SemanticError as e:
        print(f"Тест 'unused_method': Обнаружена другая ошибка - {e.message}")
        return False

def test_undefined_method():
    """Тест обнаружения ошибки вызова неопределенного метода"""
    # Создаем позицию для координат
    pos = pe.Position(1, 1)
    
    # Создаем спецификацию с ошибкой - вызов неопределенного метода
    spec = Spec(
        class_name="TestParser",
        class_name_coord=pos,
        tokens=["NUMBER", "PLUS"],
        tokens_coords=[pos, pos],
        types=[
            TypeMapping(names=["Expr"], names_coords=[pos], type=TypeName("ArithmExpr", pos), colon_coord=pos),
        ],
        methods=[],
        grammar=[
            GrammarRule(name="Expr", name_coord=pos, alternatives=[
                RuleAlternative(elements=[IdRuleElement("NUMBER", pos)], action="undefined_method", action_coord=pos)
            ])
        ],
        axiom="Expr",
        axiom_coord=pos
    )
    
    # Проверяем семантику
    try:
        analyze(spec)
        print("Ошибка! Семантический анализатор не обнаружил ошибку вызова неопределенного метода.")
        return False
    except ActionNotDefinedError as e:
        print(f"Тест 'undefined_method': Ошибка обнаружена - {e.message}")
        return True
    except SemanticError as e:
        print(f"Тест 'undefined_method': Обнаружена другая ошибка - {e.message}")
        return False

def test_array_type_for_rep():
    """Тест обнаружения ошибки типа элемента с %rep"""
    # Создаем позицию для координат
    pos = pe.Position(1, 1)
    
    # Создаем спецификацию с ошибкой - тип элемента с %rep не является массивом
    spec = Spec(
        class_name="TestParser",
        class_name_coord=pos,
        tokens=["NUMBER", "PLUS", "FRAC"],
        tokens_coords=[pos, pos, pos],
        types=[
            TypeMapping(names=["Expr"], names_coords=[pos], type=TypeName("ArithmExpr", pos), colon_coord=pos),
            TypeMapping(names=["FRAC"], names_coords=[pos], type=TypeName("ArithmOp", pos), colon_coord=pos),
        ],
        methods=[],
        grammar=[
            GrammarRule(name="Expr", name_coord=pos, alternatives=[
                RuleAlternative(elements=[IdRuleElement("NUMBER", pos)])
            ]),
            # Ошибка: тип элемента с %rep должен быть массивом
            GrammarRule(name="BadRule", name_coord=pos, alternatives=[
                RuleAlternative(elements=[
                    RepRuleElement(IdRuleElement("FRAC", pos), pos)
                ])
            ])
        ],
        axiom="Expr",
        axiom_coord=pos
    )
    
    # Проверяем семантику
    try:
        analyze(spec)
        print("Ошибка! Семантический анализатор не обнаружил ошибку типа элемента с %rep.")
        return False
    except ArrayTypeForRepError as e:
        print(f"Тест 'array_type_for_rep': Ошибка обнаружена - {e.message}")
        return True
    except SemanticError as e:
        print(f"Тест 'array_type_for_rep': Обнаружена другая ошибка - {e.message}")
        return False

def run_tests():
    """Запускает все тесты семантического анализатора"""
    print("=== Тестирование семантического анализатора ===")
    
    print("\n1. Тест на дублирование типа:")
    success1 = test_duplicate_type()
    
    print("\n2. Тест на правило для токена:")
    success2 = test_token_in_grammar()
    
    print("\n3. Тест на неиспользуемый метод:")
    success3 = test_unused_method()
    
    print("\n4. Тест на вызов неопределенного метода:")
    success4 = test_undefined_method()
    
    print("\n5. Тест на тип элемента с %rep:")
    success5 = test_array_type_for_rep()
    
    # Выводим общий результат
    print("\n=== Результат тестирования ===")
    if success1 and success2 and success3 and success4 and success5:
        print("Все тесты успешно пройдены! Семантический анализатор работает корректно.")
        return 0
    else:
        print("Некоторые тесты не пройдены. Требуется доработка семантического анализатора.")
        return 1

if __name__ == "__main__":
    sys.exit(run_tests()) 