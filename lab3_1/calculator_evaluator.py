from common import EPSILON

def evaluate_expression_tree(node):
    """
    Обходит дерево разбора арифметического выражения и вычисляет его значение.
    Предполагает, что 'n' это 1 для простоты.
    """
    if node is None:
        return 0

    # Если узел - терминал 'n'
    if node.symbol == 'n' and node.token is not None:
        # Можно было бы пытаться преобразовать node.token.value в int,
        # но для грамматики с 'n' просто вернем 1.
        try:
            return int(node.token.value) # Если 'n' может быть числом
        except ValueError:
            return 1 # Если 'n' это просто символ, как в вашей грамматике

    # Если узел - нетерминал
    # Для простоты, будем смотреть на структуру, соответствующую грамматике E, T, F
    # E -> T E'
    # E' -> + T E' | ε
    # T -> F T'
    # T' -> * F T' | ε
    # F -> n | ( E )

    if node.symbol == 'E':
        if len(node.children) >= 2 and node.children[0].symbol == 'T' and node.children[1].symbol == 'E_prime':
            t_val = evaluate_expression_tree(node.children[0])
            e_prime_val = _evaluate_e_prime(t_val, node.children[1])
            return e_prime_val
        elif len(node.children) == 1 and node.children[0].symbol == 'T': # Если E' -> ε сразу
             return evaluate_expression_tree(node.children[0])


    elif node.symbol == 'T':
        if len(node.children) >= 2 and node.children[0].symbol == 'F' and node.children[1].symbol == 'T_prime':
            f_val = evaluate_expression_tree(node.children[0])
            t_prime_val = _evaluate_t_prime(f_val, node.children[1])
            return t_prime_val
        elif len(node.children) == 1 and node.children[0].symbol == 'F': # Если T' -> ε сразу
             return evaluate_expression_tree(node.children[0])


    elif node.symbol == 'F':
        if not node.children: return 0 # Ошибка или пусто
        
        child = node.children[0]
        if child.symbol == 'n': # F -> n
            return evaluate_expression_tree(child)
        elif child.symbol == '(' : # F -> ( E )
            # Ожидаем '(' E ')'
            if len(node.children) == 3 and node.children[1].symbol == 'E':
                return evaluate_expression_tree(node.children[1])
            else:
                # Попытка найти E среди детей, если структура дерева немного иная
                e_node = next((c for c in node.children if c.symbol == 'E'), None)
                if e_node: return evaluate_expression_tree(e_node)


    # Если дошли сюда, не смогли вычислить (например, для E_prime, T_prime напрямую или ошибка)
    # print(f"Warning: Could not evaluate node: {node}")
    return 0 # Или бросить исключение

def _evaluate_e_prime(left_operand_val, e_prime_node):
    # E' -> + T E' | ε
    if not e_prime_node.children or e_prime_node.children[0].symbol == EPSILON:
        return left_operand_val # ε правило

    # E' -> + T E'
    # Дети: '+', T, E'
    if len(e_prime_node.children) == 3 and e_prime_node.children[0].symbol == '+':
        t_node = e_prime_node.children[1]
        next_e_prime_node = e_prime_node.children[2]
        
        t_val = evaluate_expression_tree(t_node)
        current_sum = left_operand_val + t_val
        return _evaluate_e_prime(current_sum, next_e_prime_node)
    
    return left_operand_val # Fallback

def _evaluate_t_prime(left_operand_val, t_prime_node):
    # T' -> * F T' | ε
    if not t_prime_node.children or t_prime_node.children[0].symbol == EPSILON:
        return left_operand_val # ε правило

    # T' -> * F T'
    # Дети: '*', F, T'
    if len(t_prime_node.children) == 3 and t_prime_node.children[0].symbol == '*':
        f_node = t_prime_node.children[1]
        next_t_prime_node = t_prime_node.children[2]

        f_val = evaluate_expression_tree(f_node)
        current_prod = left_operand_val * f_val
        return _evaluate_t_prime(current_prod, next_t_prime_node)
        
    return left_operand_val # Fallback