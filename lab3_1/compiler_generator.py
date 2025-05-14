# compiler_generator.py
import sys
import argparse
from common import generate_dot, ParseError, GrammarError, EOF_TOKEN_TYPE # EOF_TOKEN_TYPE для gdl_parser_hardcoded
from gdl_lexer import lex_gdl_tokens # <--- Убедитесь, что этот импорт есть и gdl_lexer.py существует
from gdl_parser_hardcoded import GDLHardcodedParser
from grammar_analyzer import (
    extract_grammar_from_tree,
    compute_first_sets,
    compute_follow_sets,
    build_ll1_table,
    format_table_as_python_code
)

def main():
    arg_parser = argparse.ArgumentParser(description="LL(1) Compiler Generator (Этап 1)")
    arg_parser.add_argument("input_gdl_file", help="Path to the GDL file describing user's grammar.")
    arg_parser.add_argument("-o", "--output_table_file", help="Path to save the Python parse table for user's grammar.")
    arg_parser.add_argument("--dot_gdl_ast", help="Output path for DOT of GDL file's AST (optional).")
    arg_parser.add_argument("--show_first_follow", action="store_true", help="Print FIRST and FOLLOW sets to stdout.")


    args = arg_parser.parse_args()

    try:
        print(f"Reading GDL from: {args.input_gdl_file}")
        with open(args.input_gdl_file, 'r', encoding='utf-8') as f:
            gdl_text_content = f.read()

        print("Lexing GDL input...")
        gdl_tokens_iter = lex_gdl_tokens(gdl_text_content) # <--- Использование импортированной функции
        
        print("Parsing GDL input (with GDLHardcodedParser)...")
        gdl_parser = GDLHardcodedParser(gdl_tokens_iter)
        gdl_ast_root = gdl_parser.parse()
        print("GDL input parsed successfully.")

        if args.dot_gdl_ast:
            print(f"Generating DOT AST for GDL structure to {args.dot_gdl_ast}...")
            dot_out = generate_dot(gdl_ast_root)
            with open(args.dot_gdl_ast, 'w', encoding='utf-8') as f_dot:
                f_dot.write(dot_out)
            print(f"DOT AST for GDL written to {args.dot_gdl_ast}")

        print("Extracting user's grammar structure from GDL AST...")
        user_grammar_data = extract_grammar_from_tree(gdl_ast_root)

        print("Computing FIRST sets for user's grammar...")
        first_sets = compute_first_sets(user_grammar_data['productions'], 
                                        user_grammar_data['non_terminals'], 
                                        user_grammar_data['terminals'],
                                        user_grammar_data['token_map'])
        if args.show_first_follow:
            import pprint
            print("\nFIRST sets:")
            pprint.pprint(first_sets, sort_dicts=True)


        print("Computing FOLLOW sets for user's grammar...")
        follow_sets = compute_follow_sets(user_grammar_data['productions'], 
                                          user_grammar_data['non_terminals'],
                                          user_grammar_data['terminals'], 
                                          user_grammar_data['axiom'], 
                                          first_sets,
                                          user_grammar_data['token_map'])
        if args.show_first_follow:
            import pprint
            print("\nFOLLOW sets:")
            pprint.pprint(follow_sets, sort_dicts=True)


        print("Building LL(1) parse table for user's grammar...")
        ll1_parse_table_for_user_grammar = build_ll1_table(user_grammar_data, first_sets, follow_sets)
        print("LL(1) parse table for user's grammar built successfully.")

        python_table_code = format_table_as_python_code(
            ll1_parse_table_for_user_grammar,
            user_grammar_data['axiom'],
            user_grammar_data['terminals'],
            user_grammar_data['non_terminals']
        )

        if args.output_table_file:
            print(f"Writing parse table and info for user's grammar to: {args.output_table_file}")
            with open(args.output_table_file, 'w', encoding='utf-8') as f_out:
                f_out.write(python_table_code + "\n")
            print(f"Table for user's grammar written to {args.output_table_file}")
        else:
            print("\n--- Generated Parse Table and Info for User's Grammar (Python Code) ---")
            print(python_table_code)
            print("--- End Generated Parse Table and Info ---")

    except FileNotFoundError:
        print(f"Error: Input file not found: {args.input_gdl_file}", file=sys.stderr)
        sys.exit(1)
    except (ParseError, GrammarError) as e: 
        print(f"\nCOMPILER GENERATOR ERROR: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"\nAn unexpected error in compiler_generator: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)

if __name__ == "__main__":
    main()