import pprint
from common import EPSILON, EOF_TOKEN_TYPE, GrammarError, Token 

def extract_grammar_from_tree(gdl_root_node):
    extracted_axiom = None
    productions = {}
    non_terminals = set()
    token_map = {}

    def find_child_by_symbol_name(node, symbol_name):
        return next((c for c in node.children if c.symbol == symbol_name), None)

    axiom_gdl_node = find_child_by_symbol_name(gdl_root_node, 'AXIOM')
    if not axiom_gdl_node or not axiom_gdl_node.children:
        first_lbracket_token_node = find_child_by_symbol_name(gdl_root_node, 'LBRACK')
        err_token_for_axiom = first_lbracket_token_node.token if first_lbracket_token_node else None
        raise GrammarError("Axiom definition ([axiom [...]}) not found.", err_token_for_axiom)
    
    ident_node_axiom = find_child_by_symbol_name(axiom_gdl_node, 'IDENT')
    if not ident_node_axiom or not ident_node_axiom.token:
        raise GrammarError("Axiom name (IDENT) not found in axiom definition.", axiom_gdl_node.children[0].token)
    
    extracted_axiom = ident_node_axiom.token.value
    if not extracted_axiom:
        raise GrammarError("Axiom name (IDENT) cannot be empty.", ident_node_axiom.token)
    non_terminals.add(extracted_axiom)
    token_map[extracted_axiom] = ident_node_axiom.token

    grammar_gdl_node = find_child_by_symbol_name(gdl_root_node, 'GRAMMAR')
    current_grammar_gdl_node = grammar_gdl_node
    
    while current_grammar_gdl_node:
        rule_def_gdl_node = find_child_by_symbol_name(current_grammar_gdl_node, 'RULE_DEFINITION')
        if not rule_def_gdl_node: break

        lhs_ident_node = find_child_by_symbol_name(rule_def_gdl_node, 'IDENT')
        if not lhs_ident_node or not lhs_ident_node.token:
            err_token = rule_def_gdl_node.children[0].token if rule_def_gdl_node.children else None
            raise GrammarError("LHS IDENT not found in rule definition.", err_token)
        
        lhs_name = lhs_ident_node.token.value
        if not lhs_name:
            raise GrammarError("LHS non-terminal name (IDENT) cannot be empty.", lhs_ident_node.token)
        non_terminals.add(lhs_name)
        if lhs_name not in token_map:
            token_map[lhs_name] = lhs_ident_node.token
        if lhs_name not in productions:
            productions[lhs_name] = []

        rule_bodies_gdl_node = find_child_by_symbol_name(rule_def_gdl_node, 'RULE_BODIES')
        if not rule_bodies_gdl_node:
             raise GrammarError("Internal error: RULE_BODIES not found.", lhs_ident_node.token)
        current_rule_bodies_gdl_node = rule_bodies_gdl_node
        
        while current_rule_bodies_gdl_node:
            rule_body_gdl_node = find_child_by_symbol_name(current_rule_bodies_gdl_node, 'RULE_BODY')
            if not rule_body_gdl_node: break
            rule_start_lbracket_node = find_child_by_symbol_name(rule_body_gdl_node, 'LBRACK')
            rule_start_token = rule_start_lbracket_node.token if rule_start_lbracket_node else None
            if not rule_start_token:
                 raise GrammarError("Internal error: LBRACK not found at start of RULE_BODY.", lhs_ident_node.token)
            rhs_symbols_gdl_node = find_child_by_symbol_name(rule_body_gdl_node, 'RHS_SYMBOLS')
            if not rhs_symbols_gdl_node:
                 raise GrammarError("Internal error: RHS_SYMBOLS not found.", rule_start_token)
            current_rhs_list = []
            current_rhs_gdl_node = rhs_symbols_gdl_node
            has_symbols_in_rhs = False
            while current_rhs_gdl_node:
                symbol_gdl_node_wrapper = find_child_by_symbol_name(current_rhs_gdl_node, 'SYMBOL')
                if not symbol_gdl_node_wrapper: 
                    if not has_symbols_in_rhs: current_rhs_list.append(EPSILON)
                    break
                has_symbols_in_rhs = True
                if not symbol_gdl_node_wrapper.children:
                    raise GrammarError("Internal error: SYMBOL node has no children.", rule_start_token)
                actual_user_symbol_gdl_node = symbol_gdl_node_wrapper.children[0]
                user_symbol_type_in_gdl = actual_user_symbol_gdl_node.symbol
                user_symbol_value = actual_user_symbol_gdl_node.token.value
                user_grammar_symbol_str = ""
                if user_symbol_type_in_gdl == 'IDENT':
                    user_grammar_symbol_str = user_symbol_value
                    if user_symbol_value == "EOF": user_grammar_symbol_str = EOF_TOKEN_TYPE
                    elif not user_symbol_value: raise GrammarError("Empty IDENT in RHS.", actual_user_symbol_gdl_node.token)
                elif user_symbol_type_in_gdl == 'KW_N': user_grammar_symbol_str = 'n'
                elif user_symbol_type_in_gdl in ['OP', 'LPAREN', 'RPAREN']: user_grammar_symbol_str = user_symbol_value
                else: raise GrammarError(f"Unknown GDL SYMBOL type '{user_symbol_type_in_gdl}'.", actual_user_symbol_gdl_node.token)
                current_rhs_list.append(user_grammar_symbol_str)
                current_rhs_gdl_node = find_child_by_symbol_name(current_rhs_gdl_node, 'RHS_SYMBOLS')
            productions[lhs_name].append((current_rhs_list, rule_start_token))
            rule_bodies_tail_gdl_node = find_child_by_symbol_name(current_rule_bodies_gdl_node, 'RULE_BODIES_TAIL')
            if rule_bodies_tail_gdl_node and find_child_by_symbol_name(rule_bodies_tail_gdl_node, 'RULE_BODY'):
                current_rule_bodies_gdl_node = rule_bodies_tail_gdl_node
            else: break
        current_grammar_gdl_node = find_child_by_symbol_name(current_grammar_gdl_node, 'GRAMMAR')

    terminals = set()
    all_rhs_symbols_with_tokens = {}
    for lhs, rules_for_lhs in productions.items():
        for rhs_list, rule_token_for_this_rhs in rules_for_lhs:
            for sym_str in rhs_list:
                if sym_str == EPSILON: continue
                all_rhs_symbols_with_tokens.setdefault(sym_str, rule_token_for_this_rhs)
                if sym_str not in non_terminals: terminals.add(sym_str)
    terminals.add(EOF_TOKEN_TYPE)

    for nt_name in non_terminals:
        if nt_name not in productions:
            is_axiom_of_empty_lang = (nt_name == extracted_axiom and not any(productions.values())) # Check if NO rules exist at all
            if not is_axiom_of_empty_lang and nt_name != extracted_axiom : # If it's not axiom of a truly empty grammar
                 raise GrammarError(f"Non-terminal '{nt_name}' is declared but has no production rules.", token_map.get(nt_name))
    
    for sym_in_rhs, first_occurrence_token in all_rhs_symbols_with_tokens.items():
        if sym_in_rhs != EPSILON and sym_in_rhs != EOF_TOKEN_TYPE and \
           sym_in_rhs not in non_terminals and sym_in_rhs not in terminals:
            raise GrammarError(f"Symbol '{sym_in_rhs}' in RHS is not recognized.", first_occurrence_token)

    return {'axiom': extracted_axiom, 'productions': productions, 'non_terminals': non_terminals, 'terminals': terminals, 'token_map': token_map}

def compute_first_sets(productions, non_terminals, terminals, token_map):
    first = {s: set() for s in list(non_terminals) + list(terminals)}
    for t in terminals: first[t] = {t}
    changed = True
    while changed:
        changed = False
        for nt_A in non_terminals:
            if nt_A not in productions and nt_A in first: continue
            elif nt_A not in productions and nt_A not in first:
                 raise GrammarError(f"INTERNAL ERROR: NT '{nt_A}' not in FIRST map.", token_map.get(nt_A))
            for rhs_list, rule_token in productions.get(nt_A, []):
                if rhs_list == [EPSILON]:
                    if EPSILON not in first[nt_A]: first[nt_A].add(EPSILON); changed = True
                    continue
                all_can_be_epsilon_in_rhs = True
                for symbol_X in rhs_list:
                    if symbol_X == EPSILON: current_sym_first = {EPSILON}
                    elif symbol_X not in first: raise GrammarError(f"Symbol '{symbol_X}' in rule for '{nt_A}' (RHS: {rhs_list}) has no FIRST set.", rule_token)
                    else: current_sym_first = first[symbol_X]
                    old_len_first_A = len(first[nt_A])
                    first[nt_A].update(s for s in current_sym_first if s != EPSILON)
                    if len(first[nt_A]) > old_len_first_A: changed = True
                    if EPSILON not in current_sym_first: all_can_be_epsilon_in_rhs = False; break
                if all_can_be_epsilon_in_rhs:
                    if EPSILON not in first[nt_A]: first[nt_A].add(EPSILON); changed = True
    return first

def compute_follow_sets(productions, non_terminals, terminals, axiom, first_sets, token_map):
    follow = {nt: set() for nt in non_terminals}
    if axiom and axiom in follow: follow[axiom].add(EOF_TOKEN_TYPE)
    elif axiom and axiom not in follow: raise GrammarError(f"Axiom '{axiom}' not in NTs for FOLLOW.", token_map.get(axiom))
    changed = True
    while changed:
        changed = False
        for nt_X in non_terminals:
            if nt_X not in productions: continue
            for rhs_list, rule_token in productions.get(nt_X, []):
                for i, symbol_B in enumerate(rhs_list):
                    if symbol_B in non_terminals:
                        first_of_beta = set()
                        beta_can_be_epsilon = True
                        for j in range(i + 1, len(rhs_list)):
                            symbol_after_B_Yj = rhs_list[j]
                            if symbol_after_B_Yj == EPSILON: current_sym_first_beta_Yj = {EPSILON}
                            elif symbol_after_B_Yj not in first_sets: raise GrammarError(f"Symbol '{symbol_after_B_Yj}' in rule for '{nt_X}' (RHS: {rhs_list}, after '{symbol_B}') has no FIRST set.", rule_token)
                            else: current_sym_first_beta_Yj = first_sets[symbol_after_B_Yj]
                            first_of_beta.update(s for s in current_sym_first_beta_Yj if s != EPSILON)
                            if EPSILON not in current_sym_first_beta_Yj: beta_can_be_epsilon = False; break
                        if beta_can_be_epsilon: first_of_beta.add(EPSILON)
                        old_len_follow_B = len(follow[symbol_B])
                        follow[symbol_B].update(s for s in first_of_beta if s != EPSILON)
                        if len(follow[symbol_B]) > old_len_follow_B: changed = True
                        if EPSILON in first_of_beta:
                            old_len_follow_B = len(follow[symbol_B])
                            if nt_X not in follow: raise GrammarError(f"INTERNAL ERROR: LHS NT '{nt_X}' not in FOLLOW sets for rule {nt_X}->{rhs_list}.", token_map.get(nt_X))
                            follow[symbol_B].update(follow[nt_X])
                            if len(follow[symbol_B]) > old_len_follow_B: changed = True
    return follow

def build_ll1_table(grammar_data, first_sets, follow_sets):
    productions = grammar_data['productions']; non_terminals = grammar_data['non_terminals']
    terminals = grammar_data['terminals']; token_map = grammar_data['token_map']
    ll1_table = {nt: {} for nt in non_terminals}
    for nt_A in non_terminals:
        if nt_A not in productions: continue
        for rhs_list, rule_token in productions.get(nt_A, []):
            first_of_rhs = set(); rhs_can_produce_epsilon = False
            if rhs_list == [EPSILON]: first_of_rhs.add(EPSILON); rhs_can_produce_epsilon = True
            else:
                all_symbols_in_rhs_can_be_epsilon = True
                for symbol_X_in_rhs in rhs_list:
                    if symbol_X_in_rhs == EPSILON: current_sym_first_for_rhs = {EPSILON}
                    elif symbol_X_in_rhs not in first_sets: raise GrammarError(f"Symbol '{symbol_X_in_rhs}' in rule '{nt_A} -> {rhs_list}' has no FIRST set.", rule_token)
                    else: current_sym_first_for_rhs = first_sets[symbol_X_in_rhs]
                    first_of_rhs.update(s for s in current_sym_first_for_rhs if s != EPSILON)
                    if EPSILON not in current_sym_first_for_rhs: all_symbols_in_rhs_can_be_epsilon = False; break
                if all_symbols_in_rhs_can_be_epsilon: first_of_rhs.add(EPSILON); rhs_can_produce_epsilon = True
            for terminal_a_in_first_of_rhs in first_of_rhs:
                if terminal_a_in_first_of_rhs == EPSILON: continue
                if terminal_a_in_first_of_rhs not in terminals: raise GrammarError(f"Symbol '{terminal_a_in_first_of_rhs}' from FIRST of '{nt_A} -> {rhs_list}' not in terminals.", rule_token)
                if terminal_a_in_first_of_rhs in ll1_table[nt_A]:
                    existing_lhs, existing_rhs = ll1_table[nt_A][terminal_a_in_first_of_rhs]
                    existing_rule_approx_token = token_map.get(existing_lhs, rule_token)
                    raise GrammarError(f"LL(1) FIRST/FIRST conflict for NT '{nt_A}' on terminal '{terminal_a_in_first_of_rhs}'.\n  Rule 1: {existing_lhs} -> {existing_rhs} (near {existing_rule_approx_token.line},{existing_rule_approx_token.col})\n  Rule 2: {nt_A} -> {rhs_list} (at {rule_token.line},{rule_token.col})", rule_token)
                ll1_table[nt_A][terminal_a_in_first_of_rhs] = (nt_A, rhs_list)
            if rhs_can_produce_epsilon:
                if nt_A not in follow_sets: raise GrammarError(f"NT '{nt_A}' (can derive ε via {nt_A}->{rhs_list}) has no FOLLOW set.", token_map.get(nt_A, rule_token))
                for terminal_b_in_follow_A in follow_sets[nt_A]:
                    if terminal_b_in_follow_A not in terminals: raise GrammarError(f"Symbol '{terminal_b_in_follow_A}' from FOLLOW of '{nt_A}' not in terminals.", token_map.get(nt_A, rule_token))
                    if terminal_b_in_follow_A in ll1_table[nt_A]:
                        existing_lhs, existing_rhs = ll1_table[nt_A][terminal_b_in_follow_A]
                        existing_rule_approx_token = token_map.get(existing_lhs, rule_token)
                        raise GrammarError(f"LL(1) conflict (likely FIRST/FOLLOW) for NT '{nt_A}' on terminal '{terminal_b_in_follow_A}' due to ε-production '{nt_A} -> {rhs_list}'.\n  Existing rule: {existing_lhs} -> {existing_rhs} (near {existing_rule_approx_token.line},{existing_rule_approx_token.col})\n  Attempting ε-rule: {nt_A} -> {rhs_list} (at {rule_token.line},{rule_token.col})", rule_token)
                    table_rhs = [EPSILON] if rhs_list != [EPSILON] else rhs_list # Ensure canonical ε-rule
                    ll1_table[nt_A][terminal_b_in_follow_A] = (nt_A, table_rhs)
    return ll1_table

def format_table_as_python_code(table, axiom_name, grammar_terminals, grammar_non_terminals):
    code = f"# Generated Parse Table and Grammar Info\n"
    code += f"EPSILON = \"{EPSILON}\"\n"
    code += f"EOF_TOKEN_TYPE = \"{EOF_TOKEN_TYPE}\"\n" # Используем константу из common
    code += f"GRAMMAR_AXIOM = \"{axiom_name}\"\n"
    code += f"GRAMMAR_TERMINALS = {pprint.pformat(set(grammar_terminals), sort_dicts=False)}\n" 
    code += f"GRAMMAR_NON_TERMINALS = {pprint.pformat(set(grammar_non_terminals), sort_dicts=False)}\n"
    code += "parse_table = " + pprint.pformat(table, indent=4, width=120, sort_dicts=True)
    return code