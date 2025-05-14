# bootstrapper.py
import sys
import argparse
import importlib.util # Для динамического импорта таблицы GDL
from common import generate_dot, ParseError, GrammarError # Для обработки ошибок и DOT
# lex_gdl_tokens нужен для лексического анализа GDL файла пользователя
from gdl_lexer import lex_gdl_tokens 
# GenericLL1Parser нужен для парсинга GDL файла пользователя по таблице GDL
from generic_ll1_parser import GenericLL1Parser 
# Функции анализа нужны для обработки AST GDL файла пользователя и построения таблицы для ЕГО грамматики
from grammar_analyzer import (
    extract_grammar_from_tree,
    compute_first_sets,
    compute_follow_sets,
    build_ll1_table,
    format_table_as_python_code
)


def main_bootstrapper():
    arg_parser = argparse.ArgumentParser(description="Bootstrapped LL(1) Compiler Generator (Этап 4).")
    arg_parser.add_argument("gdl_parse_table_file", help="Path to the Python file with GDL's own parse table (e.g., gdl_table.py).")
    arg_parser.add_argument("input_user_grammar_file", help="Path to the GDL file describing the user's grammar (e.g., calculator_grammar.gdl).")
    arg_parser.add_argument("-o", "--output_user_table_file", help="Path to save the generated Python parse table for the user's grammar.")
    arg_parser.add_argument("--dot_user_gdl_ast", help="Output path for DOT of the user grammar's GDL AST built by bootstrapper (optional).")
    arg_parser.add_argument("--show_first_follow", action="store_true", help="Print FIRST/FOLLOW for the user's grammar.")


    args = arg_parser.parse_args()

    # 1. Загрузка таблицы разбора для самого GDL
    try:
        spec = importlib.util.spec_from_file_location("gdl_table_module", args.gdl_parse_table_file)
        if spec is None or spec.loader is None:
            raise ImportError(f"Could not load spec for GDL table module from {args.gdl_parse_table_file}")
        gdl_table_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(gdl_table_module) # Выполняем код модуля таблицы
        print(f"GDL's own parse table loaded from: {args.gdl_parse_table_file}")
        # Проверяем наличие необходимых атрибутов в загруженном GDL модуле
        required_gdl_attrs = ['parse_table', 'GRAMMAR_AXIOM', 'GRAMMAR_TERMINALS', 'GRAMMAR_NON_TERMINALS', 'EPSILON', 'EOF_TOKEN_TYPE']
        for attr in required_gdl_attrs:
            if not hasattr(gdl_table_module, attr):
                raise ImportError(f"GDL table file '{args.gdl_parse_table_file}' is missing attribute: {attr}")

    except Exception as e:
        print(f"Error loading GDL's parse table module: {e}", file=sys.stderr)
        sys.exit(1)

    # 2. Чтение GDL-файла, описывающего грамматику пользователя
    try:
        with open(args.input_user_grammar_file, 'r', encoding='utf-8') as f:
            user_gdl_text = f.read()
        print(f"Reading user's grammar definition (GDL) from: {args.input_user_grammar_file}")
    except FileNotFoundError:
        print(f"Error: User's grammar GDL file not found: {args.input_user_grammar_file}", file=sys.stderr)
        sys.exit(1)

    try:
        # 3. Лексический анализ GDL-файла пользователя (используем gdl_lexer.py)
        print("Lexing user's GDL input...")
        user_gdl_tokens_iter = lex_gdl_tokens(user_gdl_text)

        # 4. Синтаксический анализ GDL-файла пользователя с помощью GenericLL1Parser
        #    и таблицы для GDL (gdl_table_module)
        print("Parsing user's GDL input (with GenericLL1Parser + GDL's table)...")
        # Передаем GenericLL1Parser'у лексер GDL и модуль с таблицей GDL
        gdl_parser_for_user_grammar = GenericLL1Parser(lex_gdl_tokens, gdl_table_module) 
        
        # Метод parse GenericLL1Parser'а вызывает self.lexer_func(text)
        user_gdl_ast_root = gdl_parser_for_user_grammar.parse(user_gdl_text) 
        print("User's GDL input parsed successfully by bootstrapped parser.")

        if args.dot_user_gdl_ast:
            print(f"Generating DOT AST for user's GDL structure (bootstrapped) to {args.dot_user_gdl_ast}...")
            dot_out = generate_dot(user_gdl_ast_root) 
            with open(args.dot_user_gdl_ast, 'w', encoding='utf-8') as f_dot:
                f_dot.write(dot_out)
            print(f"DOT AST for user's GDL written to {args.dot_user_gdl_ast}")

        # 5. Остальная часть - как в compiler_generator.py: извлечь грамматику пользователя, построить таблицу для нее
        print("Extracting user's grammar structure from its GDL AST...")
        # user_gdl_ast_root - это дерево разбора, построенное GenericLL1Parser'ом
        # (он использует имена NT из gdl_table.py, которые мы синхронизировали)
        final_user_grammar_data = extract_grammar_from_tree(user_gdl_ast_root)

        print("Computing FIRST sets for final user's grammar...")
        first_sets = compute_first_sets(final_user_grammar_data['productions'], 
                                        final_user_grammar_data['non_terminals'], 
                                        final_user_grammar_data['terminals'],
                                        final_user_grammar_data['token_map'])
        if args.show_first_follow:
            import pprint
            print("\nFIRST sets for user's grammar:")
            pprint.pprint(first_sets, sort_dicts=True)

        print("Computing FOLLOW sets for final user's grammar...")
        follow_sets = compute_follow_sets(final_user_grammar_data['productions'], 
                                          final_user_grammar_data['non_terminals'],
                                          final_user_grammar_data['terminals'], 
                                          final_user_grammar_data['axiom'], 
                                          first_sets,
                                          final_user_grammar_data['token_map'])
        if args.show_first_follow:
            import pprint
            print("\nFOLLOW sets for user's grammar:")
            pprint.pprint(follow_sets, sort_dicts=True)


        print("Building LL(1) parse table for final user's grammar...")
        ll1_parse_table_for_user = build_ll1_table(final_user_grammar_data, first_sets, follow_sets)
        print("LL(1) parse table for user's grammar built successfully by bootstrapped generator.")

        python_table_code = format_table_as_python_code(
            ll1_parse_table_for_user,
            final_user_grammar_data['axiom'],
            final_user_grammar_data['terminals'],
            final_user_grammar_data['non_terminals']
        )

        if args.output_user_table_file:
            print(f"Writing parse table for user's grammar to: {args.output_user_table_file}")
            with open(args.output_user_table_file, 'w', encoding='utf-8') as f_out:
                f_out.write(python_table_code + "\n")
            print(f"Parse table for user's grammar written to {args.output_user_table_file}")
        else:
            print("\n--- Generated Parse Table for User's Grammar (by bootstrapped generator) ---")
            print(python_table_code)
            print("--- End Generated Parse Table ---")

    except FileNotFoundError: # Уже обработано для файлов аргументов
        pass 
    except (ParseError, GrammarError) as e:
        print(f"\nBOOTSTRAPPER ERROR: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"\nAn unexpected error in bootstrapper: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main_bootstrapper()