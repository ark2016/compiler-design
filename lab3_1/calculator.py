# calculator.py
import sys
import argparse
import importlib.util # Для динамического импорта модуля таблицы
from common import generate_dot, ParseError, GrammarError # Исключения для try-except
from calculator_lexer import lex_calculator_tokens
from generic_ll1_parser import GenericLL1Parser
from calculator_evaluator import evaluate_expression_tree # Опционально

def main_calculator():
    parser = argparse.ArgumentParser(description="Calculator using a generated LL(1) parse table.")
    parser.add_argument("parse_table_file", help="Path to the Python file with parse_table, axiom, etc.")
    parser.add_argument("input_expression_file", help="Path to file with arithmetic expressions.")
    parser.add_argument("-o", "--output_dot_file", help="Path to save DOT parse tree (optional).")
    parser.add_argument("--eval", action="store_true", help="Evaluate the parsed expression.")

    args = parser.parse_args()

    # 1. Загрузка таблицы разбора и информации о грамматике
    try:
        spec = importlib.util.spec_from_file_location("generated_table_module", args.parse_table_file)
        if spec is None or spec.loader is None: # Добавил проверку spec.loader
            raise ImportError(f"Could not load spec for module from {args.parse_table_file}")
        
        table_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(table_module) # Выполняем код модуля таблицы
        print(f"Parse table module loaded from: {args.parse_table_file}")
    except FileNotFoundError:
        print(f"Error: Parse table file not found: {args.parse_table_file}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error loading parse table module from '{args.parse_table_file}': {e}", file=sys.stderr)
        sys.exit(1)

    # Проверка наличия необходимых атрибутов в загруженном модуле
    required_attrs = ['parse_table', 'GRAMMAR_AXIOM', 'GRAMMAR_TERMINALS', 
                      'GRAMMAR_NON_TERMINALS', 'EPSILON', 'EOF_TOKEN_TYPE']
    for attr in required_attrs:
        if not hasattr(table_module, attr):
            print(f"Error: The table file '{args.parse_table_file}' is missing attribute: {attr}", file=sys.stderr)
            sys.exit(1)

    # 2. Чтение входного выражения
    try:
        with open(args.input_expression_file, 'r', encoding='utf-8') as f:
            expression_text = f.read()
        print(f"Read expression from: {args.input_expression_file}")
    except FileNotFoundError:
        print(f"Error: Input expression file not found: {args.input_expression_file}", file=sys.stderr)
        sys.exit(1)

    # 3. Инициализация и запуск парсера
    try:
        print("Initializing calculator's GenericLL1Parser...")
        # Передаем сам модуль table_module, чтобы парсер мог получить все нужные атрибуты
        calc_parser = GenericLL1Parser(lex_calculator_tokens, table_module) 
        
        print(f"Parsing expression: '{expression_text.strip()}'")
        parse_tree_root = calc_parser.parse(expression_text) # Здесь может быть ParseError

        if args.output_dot_file:
            print(f"Writing calculator parse tree to: {args.output_dot_file}")
            dot_output = generate_dot(parse_tree_root)
            with open(args.output_dot_file, 'w', encoding='utf-8') as f_out:
                f_out.write(dot_output)
            print("Calculator DOT parse tree written.")
        else:
            print("\n--- Calculator Parse Tree (DOT format preview) ---")
            dot_preview = generate_dot(parse_tree_root)
            print(dot_preview[:1000] + ("..." if len(dot_preview) > 1000 else ""))
            print("--- End Calculator Parse Tree Preview ---")

        if args.eval:
            if parse_tree_root:
                print("\nEvaluating expression...")
                # Для простоты, если 'n' всегда 1.
                # Если 'n' может быть разным, evaluator нужно доработать.
                # Также evaluator должен быть устойчив к разным структурам дерева,
                # которые может породить парсер (например, с лишними узлами ε).
                try:
                    result = evaluate_expression_tree(parse_tree_root)
                    print(f"Evaluation result: {result}")
                except Exception as eval_e:
                    print(f"Error during evaluation: {eval_e}")
            else:
                print("Cannot evaluate: Parse tree was not successfully built.")


    except (ParseError, GrammarError) as e: # Ловим ошибки из лексера, парсера
        print(f"\nCALCULATOR PROCESSING ERROR: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e: # Другие неожиданные ошибки
        print(f"\nAn unexpected error in calculator: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main_calculator()