from collections import namedtuple

EPSILON = "ε"
EOF_TOKEN_TYPE = "EOF"

Token = namedtuple('Token', ['type', 'value', 'line', 'col'])

class Node:
    _ids = 0
    def __init__(self, symbol, token=None):
        self.symbol = symbol
        self.token = token
        self.children = []
        self.id = f"n{Node._ids}"
        Node._ids += 1

    def add_child(self, node):
        if node:
            self.children.append(node)

    def __repr__(self):
        if self.token:
            return f"Node(token={self.token.type}='{self.token.value}' ({self.token.line},{self.token.col}), id={self.id})"
        else:
            return f"Node(symbol='{self.symbol}', id={self.id})"

class ParseError(Exception):
    def __init__(self, message, token=None):
        self.token = token
        if token and token.type != EOF_TOKEN_TYPE :
            super().__init__(f"{message} (at line {token.line}, col {token.col}, token: '{token.value}' [{token.type}])")
        elif token and token.type == EOF_TOKEN_TYPE:
            super().__init__(f"{message} (at end of input, line {token.line}, col {token.col})")
        else:
            super().__init__(message)

class GrammarError(Exception):
    def __init__(self, message, token=None):
        self.token = token
        if token:
            super().__init__(f"{message} (near line {token.line}, col {token.col}, related token: '{token.value}')")
        else:
            super().__init__(message)

def _get_node_label(node):
    def escape(s):
        return str(s).replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
    if node.symbol == EPSILON: return "ε"
    if node.token is None and node.symbol != EOF_TOKEN_TYPE: return escape(node.symbol)
    if node.token:
        val, type_esc = escape(node.token.value), escape(node.token.type)
        if type_esc == 'LBRACK': return "["
        if type_esc == 'RBRACK': return "]"
        if type_esc == 'LPAREN': return "("
        if type_esc == 'RPAREN': return ")"
        if type_esc == 'OP': return f"{val}"
        if type_esc == 'KW_AXIOM': return "axiom"
        if type_esc == 'KW_N': return "n"
        if type_esc == 'IDENT': return f"{val}"
        if type_esc == EOF_TOKEN_TYPE: return "EOF"
        return f"{type_esc}\\n({val})" if val else type_esc
    if node.symbol == EOF_TOKEN_TYPE: return "EOF"
    return escape(node.symbol)

def generate_dot(root_node):
    if not root_node: return "digraph G {}"
    dot_nodes, dot_edges, rank_constraints = [], [], []
    
    # Сброс и переназначение ID для консистентности при нескольких вызовах
    _node_ids_temp_store = Node._ids 
    Node._ids = 0 
    
    queue_for_id_reset = [root_node]
    visited_for_id_reset = {root_node.id} # Используем исходные ID для отслеживания посещенных
    
    # Создаем карту старых ID на новые узлы, чтобы сохранить структуру, если узлы дублируются
    node_map_by_old_id = {}

    new_root_id = f"n{Node._ids}"
    Node._ids += 1
    node_map_by_old_id[root_node.id] = (new_root_id, root_node) # (new_id, original_node)


    processed_for_id_reset = set()
    bfs_q = [root_node]
    
    # Сначала обойдем дерево и назначим новые ID всем уникальным узлам
    all_nodes_in_tree = []
    temp_q = [root_node]
    visited_temp = set()
    while temp_q:
        curr = temp_q.pop(0)
        if curr in visited_temp: continue
        visited_temp.add(curr)
        all_nodes_in_tree.append(curr)
        for child in curr.children:
            if child: temp_q.append(child)

    for n_idx, node_in_tree in enumerate(all_nodes_in_tree):
        node_in_tree.id = f"n{n_idx}" # Просто перенумеруем все узлы в порядке BFS

    Node._ids = len(all_nodes_in_tree) # Обновим счетчик Node


    queue, processed_nodes = [root_node], set()
    while queue:
        current_node = queue.pop(0)
        if current_node.id in processed_nodes: continue
        processed_nodes.add(current_node.id)
        label = _get_node_label(current_node)
        dot_nodes.append(f'  {current_node.id} [label="{label}"];')
        child_ids = []
        valid_children = [child for child in current_node.children if child]
        for child in valid_children:
            dot_edges.append(f"  {current_node.id} -> {child.id};")
            child_ids.append(child.id)
            if child.id not in processed_nodes and child not in queue:
                queue.append(child)
        if len(child_ids) > 1:
             invisible_edges = " -> ".join(child_ids)
             rank_constraints.append(f"  {{ rank=same; {invisible_edges} [style=invis]; }}")
    
    Node._ids = _node_ids_temp_store # Восстанавливаем старое значение _ids, если это важно для других частей

    return "digraph {\n  rankdir=TB;\n  node [shape=ellipse];\n" + \
           "\n".join(dot_nodes) + "\n\n" + \
           "\n".join(dot_edges) + "\n\n" + \
           "\n".join(rank_constraints) + "\n}"