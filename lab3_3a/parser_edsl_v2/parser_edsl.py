import abc
import collections
import dataclasses
import re
import sys


__all__ = '''
Terminal
ExAction
NonTerminal
EOF_SYMBOL
Position
Fragment
Parser
Error
Left
Right
NonAssoc
TokenAttributeError
'''.split()


@dataclasses.dataclass(frozen=True)
class Precedence:
    level: int
    associativity: str

    def __repr__(self):
        return f"Precedence({self.associativity!r}, {self.level!r})"


@dataclasses.dataclass(frozen=True)
class Left(Precedence):
    level: int
    associativity: str = 'left'


@dataclasses.dataclass(frozen=True)
class Right(Precedence):
    level: int
    associativity: str = 'right'


@dataclasses.dataclass(frozen=True)
class NonAssoc(Precedence):
    level: int
    associativity: str = 'nonassoc'


class Symbol:
    pass


class BaseTerminal(Symbol):
    pass


def pos_from_offset(text, offset):
    line = text.count('\n', 0, offset) + 1
    last_newline = text.rfind('\n', 0, offset)
    col = offset - last_newline if last_newline != -1 else offset + 1
    return Position(offset, line, col)


class Terminal(BaseTerminal):
    def __init__(self, name, regex, func, *, priority=5, re_flags=re.MULTILINE):
        self.name = name
        self.regex = regex
        self.func = func
        self.priority = priority
        self.re = re.compile(regex, re_flags)

    def __repr__(self):
        return f'Terminal({self.name!r},{self.regex!r},{self.func!r})'

    def __str__(self):
        return self.name

    def match(self, string, pos):
        m = self.re.match(string, pos)
        if m != None:
            begin, end = m.span()
            try:
                attrib = self.func(string[begin:end])
            except TokenAttributeError as exc:
                raise LexerError(pos_from_offset(string, begin), string, message=exc.message) from exc
            return end - begin, attrib
        else:
            return 0, None


class LiteralTerminal(BaseTerminal):
    def __init__(self, image):
        self.image = image
        self.priority = 10

    def __hash__(self):
        return hash(self.image)

    def __eq__(self, other):
        return type(self) == type(other) and self.image == other.image

    def __repr__(self):
        return f'LiteralTerminal({self.image!r})'

    def __str__(self):
        return repr(self.image)

    def match(self, string, pos):
        if string.startswith(self.image, pos):
            return len(self.image), None
        else:
            return 0, None


class SpecTerminal(BaseTerminal):
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return f'SpecTerminal({self.name})'

    def __str__(self):
        return self.name


EOF_SYMBOL = SpecTerminal('EOF')
FREE_SYMBOL = SpecTerminal('#')


class ErrorTerminal(BaseTerminal):
    priority = -1

    @staticmethod
    def match(string, pos):
        assert pos < len(string)
        return 1, ErrorTerminal


@dataclasses.dataclass(frozen = True)
class ExAction:
    callee : object

    @staticmethod
    def wrap_simple_action(simple_fold):
        def extended_action(attrs, coords, res_coord):
            return simple_fold(*attrs)

        return ExAction(extended_action)


class NonTerminal(Symbol):
    def __init__(self, name):
        self.name = name
        self.productions = []
        self.lambdas = []
        self.precedence_info = []

    def __repr__(self):
        return 'NonTerminal(' + repr(self.name) + ')'

    def __str__(self):
        return self.name

    def stringify(self, pretty=True):
        title = '%s: ' % self.name

        if pretty:
            separator = '\n%s| ' % (' ' * len(self.name))
        else:
            separator = ''

        def strprod(prod):
            return ' '.join(str(sym) for sym in prod)

        rules = separator.join(strprod(prod) for prod in self.productions)
        return title + rules


    @staticmethod
    def __wrap_literals(symbol):
        if isinstance(symbol, str):
            return LiteralTerminal(symbol)
        else:
            assert isinstance(symbol, Symbol)
            return symbol


    def __ior__(self, other):
        is_callable = lambda obj: hasattr(obj, '__call__')
        is_fold = lambda obj: is_callable(obj) or isinstance(obj, ExAction)
        is_precedence = lambda obj: isinstance(obj, Precedence)

        if other == ():
            self |= lambda: None
        elif isinstance(other, tuple) and len(other) >= 2 and is_precedence(other[-2]):
            # if precedence rule
            *symbols, prec, fold = other
            symbols = [self.__wrap_literals(sym) for sym in symbols]
            symbols[1].priority = prec.level
            if callable(fold):
                fold = ExAction.wrap_simple_action(fold)

            self.productions.append(symbols)
            self.lambdas.append(fold)
            self.precedence_info.append(prec)
        elif isinstance(other, tuple) and (isinstance(other[-1], ExAction)
                  or (callable(other[-1]) and not isinstance(other[-1], Precedence))):
            *symbols, fold = other
            symbols = [self.__wrap_literals(sym) for sym in symbols]
            if callable(fold):
                fold = ExAction.wrap_simple_action(fold)

            self.productions.append(symbols)
            self.lambdas.append(fold)
            self.precedence_info.append(None)
        elif isinstance(other, tuple):
                self |= other + (self.__default_fold,)
        elif isinstance(other, Symbol) or is_fold(other):
            self |= (other,)
        elif isinstance(other, str):
            self |= (LiteralTerminal(other),)
        else:
            raise Exception('Bad rule')

        return self

    @staticmethod
    def __default_fold(*args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 0:
            return None
        else:
            raise RuntimeError('__default_fold', args)

    def enum_rules(self, include_precedence = True):
        if include_precedence:
            return zip(self.productions, self.lambdas, self.precedence_info)
        return zip(self.productions, self.lambdas)


@dataclasses.dataclass(frozen = True)
class Position:
    offset : int = 0
    line : int = 1
    col : int = 1

    def shift(self, text : str):
        offset, line, col = dataclasses.astuple(self)

        for char in text:
            if char == '\n':
                line += 1
                col = 1
            else:
                col += 1

        return Position(offset + len(text), line, col)

    def __str__(self):
        return f'({self.line}, {self.col})'


@dataclasses.dataclass(frozen = True)
class Fragment:
    start : Position
    following : Position

    def __str__(self):
        return f'{self.start}-{self.following}'


@dataclasses.dataclass
class Token:
    type : BaseTerminal
    pos : Fragment
    attr : object

    def __str__(self):
        if self.attr is not None:
            return f'{self.type}({self.attr})'
        else:
            return str(self.type)


class LrZeroItemTableEntry:
    def __init__(self):
        self.propagates_to = set()
        self.lookaheads = set()

    def __repr__(self):
        pattern = '{ propagatesTo: %s, lookaheads: %s }'
        return pattern % (repr(self.propagates_to), repr(self.lookaheads))


Shift = collections.namedtuple('Shift', 'state')
Reduce = collections.namedtuple('Reduce', 'rule')
Accept = collections.namedtuple('Accept', '')


class ParsingTable:
    def __init__(self, gr):
        self.grammar = gr

        self.terminals = ()
        self.nonterms = ()
        self.__ccol = ()
        self.n_states = 0

        self.goto = ()
        self.action = ()

        self.__setup_from_grammar(self.grammar)

    def __setup_from_grammar(self, gr):
        self.terminals = gr.terminals + tuple([EOF_SYMBOL])
        self.nonterms = gr.nonterms[1:]

        self.__ccol = tuple(get_canonical_collection(gr))
        self.n_states = len(self.__ccol)

        ccol_core = tuple(drop_itemset_lookaheads(x) for x in self.__ccol)
        id_from_core = {ccol_core[i]: i for i in range(len(self.__ccol))}

        self.goto = tuple({x: None for x in self.nonterms} for i in range(self.n_states))
        self.action = tuple({x: set() for x in self.terminals} for i in range(self.n_states))

        goto_precalc = tuple(dict() for i in range(self.n_states))
        for symbol in (self.terminals + self.nonterms):
            for state_id in range(self.n_states):
                next_state = goto(gr, self.__ccol[state_id], symbol)
                if len(next_state) == 0:
                    continue
                next_state_id = id_from_core[drop_itemset_lookaheads(next_state)]
                goto_precalc[state_id][symbol] = next_state_id

        for state_id in range(self.n_states):
            for item, next_symbol in self.__ccol[state_id]:
                prod_index, dot = item
                _, pbody, _, _ = gr.productions[prod_index]

                if dot < len(pbody):
                    terminal = pbody[dot]
                    if not isinstance(terminal, BaseTerminal) or terminal not in goto_precalc[state_id]:
                        continue

                    next_state_id = goto_precalc[state_id][terminal]
                    self.action[state_id][terminal].add(Shift(next_state_id))
                else:
                    if prod_index == 0:
                        assert (next_symbol == EOF_SYMBOL)
                        self.action[state_id][EOF_SYMBOL].add(Accept())
                    else:
                        self.action[state_id][next_symbol].add(Reduce(prod_index))

            for nt in self.nonterms:
                if nt not in goto_precalc[state_id]:
                    continue
                next_state_id = goto_precalc[state_id][nt]
                self.goto[state_id][nt] = next_state_id

    @staticmethod
    def __stringify_action_entries(term, ent):
        return '\tfor terminal %s: ' % term + ', '.join(map(str, ent))

    @staticmethod
    def __stringify_goto_entry(nt, sid):
        return '\tfor non-terminal %s: go to state %d' % (str(nt), sid)

    def __stringify_lr_zero_item(self, item):
        prod_index, dot = item
        pname, pbody, _, _ = self.grammar.productions[prod_index]
        dotted_pbody = pbody[:dot] + ['.'] + pbody[dot:]
        dotted_pbody_str = ' '.join(str(x) for x in dotted_pbody)
        return RULE_INDEXING_PATTERN % (prod_index, pname.name + ': ' + dotted_pbody_str)

    def stringify_state(self, state_id):
        state_title = 'State %d\n' % state_id
        items = drop_itemset_lookaheads(kernels(self.__ccol[state_id]))
        items = sorted(items, key=lambda elem: elem[0])
        items_str = '\n'.join('\t' + self.__stringify_lr_zero_item(item) for item in items) + '\n\n'
        # TODO CHANGED FOR TERMINALS MAYBE WRONG
        actions = [(t, e) for t, e in self.action[state_id].items() if len(e) > 0]
        actions_str = '\n'.join(self.__stringify_action_entries(t, e) for t, e in actions)
        actions_str += ('\n' if len(actions_str) > 0 else '')

        gotos = [(nt, sid) for nt, sid in self.goto[state_id].items() if sid is not None]
        gotos = sorted(gotos, key=lambda elem: elem[0].name)

        gotos_str = '\n'.join(self.__stringify_goto_entry(nt, sid) for nt, sid in gotos)
        gotos_str += ('\n' if len(gotos_str) > 0 else '')

        action_goto_separator = ('\n' if len(actions_str) > 0 and len(gotos_str) > 0 else '')
        return state_title + items_str + actions_str + action_goto_separator + gotos_str

    def stringify(self):
        states_str = '\n'.join(self.stringify_state(i) for i in range(self.n_states))
        return states_str

    @staticmethod
    def __get_entry_status(e):
        if len(e) <= 1:
            return STATUS_OK
        n_actions = len(frozenset(type(a) for a in e))
        return STATUS_SR_CONFLICT if n_actions == 2 else STATUS_RR_CONFLICT

    def get_single_state_conflict_status(self, state_id):
        seq = [self.__get_entry_status(e) for t, e in self.action[state_id].items()]
        return STATUS_OK if len(seq) == 0 else max(seq)

    def get_conflict_status(self):
        return [self.get_single_state_conflict_status(i) for i in range(self.n_states)]

    def is_lalr_one(self):
        seq = self.get_conflict_status()
        return (STATUS_OK if len(seq) == 0 else max(seq)) == STATUS_OK


def get_canonical_collection(gr):
    dfa = LR0_Automaton(gr)
    kstates = dfa.kstates()
    n_states = len(kstates)

    table = [{item: LrZeroItemTableEntry() for item in kstates[i]} for i in range(n_states)]
    table[0][(0, 0)].lookaheads.add(EOF_SYMBOL)

    for i_state_id in range(n_states):
        state_symbols = [x[1] for x, y in dfa.goto.items() if x[0] == i_state_id]

        for i_item in kstates[i_state_id]:
            closure_set = closure(gr, [(i_item, FREE_SYMBOL)])

            for sym in state_symbols:
                j_state_id = dfa.goto[(i_state_id, sym)]

                # For each item in closure_set whose . (dot) points to a symbol equal to 'sym'
                # i.e. a production expecting to see 'sym' next
                for ((prod_index, dot), next_symbol) in closure_set:
                    _, pbody, _, _ = gr.productions[prod_index]
                    if dot == len(pbody) or pbody[dot] != sym:
                        continue

                    j_item = (prod_index, dot + 1)
                    if next_symbol == FREE_SYMBOL:
                        table[i_state_id][i_item].propagates_to.add((j_state_id, j_item))
                    else:
                        table[j_state_id][j_item].lookaheads.add(next_symbol)

    repeat = True
    while repeat:
        repeat = False
        for i_state_id in range(len(table)):
            for i_item, i_cell in table[i_state_id].items():
                # For every kernel item i_item's lookaheads propagate to
                for j_state_id, j_item in i_cell.propagates_to:
                    j_cell = table[j_state_id][j_item]
                    j_cell_lookaheads_len = len(j_cell.lookaheads)
                    j_cell.lookaheads.update(i_cell.lookaheads)
                    if j_cell_lookaheads_len < len(j_cell.lookaheads):
                        repeat = True

    result = [set() for i in range(n_states)]
    for i_state_id in range(n_states):
        for i_item, i_cell in table[i_state_id].items():
            for sym in i_cell.lookaheads:
                item_set = (i_item, sym)
                result[i_state_id].add(item_set)
        result[i_state_id] = closure(gr, result[i_state_id])

    return result


def closure(gr, item_set):
    result = set(item_set)
    current = item_set

    while len(current) > 0:
        new_elements = []
        for ((prod_index, dot), lookahead) in current:
            _, pbody, _, _ = gr.productions[prod_index]
            if dot == len(pbody) or pbody[dot] not in gr.nonterms:
                continue
            nt = pbody[dot]
            nt_offset = gr.nonterm_offset[nt]
            following_symbols = pbody[dot + 1:] + [lookahead]
            following_terminals = gr.first_set(following_symbols) - {None}
            for idx in range(len(nt.productions)):
                for term in following_terminals:
                    new_item_set = ((nt_offset + idx, 0), term)
                    if new_item_set not in result:
                        result.add(new_item_set)
                        new_elements += [new_item_set]
        current = new_elements
    return frozenset(result)


class Error(Exception, abc.ABC):
    @abc.abstractproperty
    def message(self):
        pass


class TokenAttributeError(Error):
    def __init__(self, text):
        self.bad = text

    def __repr__(self):
        return f'TokenAttributeError({self.pos!r}, {self.bad!r})'

    @property
    def message(self):
        return f'{self.bad}'


@dataclasses.dataclass
class ParseError(Error):
    pos : Position
    unexpected : Symbol
    expected : list
    _text: str = ""

    @property
    def message(self):
        if self._text != "":
            return self._text
        expected = ', '.join(map(str, self.expected))
        return (f'Неожиданный символ {self.unexpected}, '
                f'ожидалось {expected}')


class PredictiveTableConflictError(Error):
    def __init__(self, nonterm, terminal, existing_rule, new_rule):
        self.nonterm = nonterm
        self.terminal = terminal
        self.existing_rule = existing_rule
        self.new_rule = new_rule

    @property
    def message(self):
        return (f'LL(1) conflict: для нетерминала {self.nonterm} и терминала {self.terminal}\n'
                f'уже есть правило {self.existing_rule}, попытка добавить {self.new_rule}')


@dataclasses.dataclass
class ParseTreeNode:
    symbol: Symbol
    fold: ExAction = None
    token: Token = None
    children: list = dataclasses.field(default_factory=list)
    attribute: object = None


class PredictiveParsingTable:
    def __init__(self, grammar):
        self.grammar = grammar
        self.table = {}

        self.follow_sets = self._build_follow_sets()
        self._build_table()

    def _build_follow_sets(self):
        follow = {nt: set() for nt in self.grammar.nonterms}
        start_nt = self.grammar.nonterms[0]

        follow[start_nt].add(EOF_SYMBOL)

        changed = True
        while changed:
            changed = False
            for (nt, prod, _, _) in self.grammar.productions:
                for i, sym in enumerate(prod):
                    if sym not in self.grammar.nonterms:
                        continue
                    beta = prod[i+1:]
                    first_of_beta = self.grammar.first_set(beta)
                    before_len = len(follow[sym])

                    without_epsilon = set(first_of_beta) - {None}
                    follow[sym].update(without_epsilon)

                    if None in first_of_beta:
                        follow[sym].update(follow[nt])

                    after_len = len(follow[sym])
                    if after_len > before_len:
                        changed = True
        return follow

    def _build_table(self):
        for nt in self.grammar.nonterms:
            self.table[nt] = {}

        for i, (nt, prod, fold, _) in enumerate(self.grammar.productions):
            fs = self.grammar.first_set(prod)
            non_epsilon = fs - {None}
            for t in non_epsilon:
                self._add_rule(nt, t, prod, fold)

            if None in fs:
                for t in self.follow_sets[nt]:
                    self._add_rule(nt, t, prod, fold)

    def _add_rule(self, nt, terminal, prod, fold):
        if terminal not in self.table[nt]:
            self.table[nt][terminal] = (prod, fold)
        else:
            existing = self.table[nt][terminal]
            raise PredictiveTableConflictError(
                nonterm=nt,
                terminal=terminal,
                existing_rule=existing,
                new_rule=(prod, fold)
            )

    def stringify(self):
        lines = []
        for nt in self.grammar.nonterms:
            row = self.table[nt]
            lines.append(f'{nt}:')
            for term, (alpha, fold) in row.items():
                alpha_str = ' '.join(str(s) for s in alpha) if alpha else 'ε'
                lines.append(f'   {term} -> {alpha_str}')
        return '\n'.join(lines)


class Parser(object):
    def __init__(self, start_nonterminal):

        fake_axiom = NonTerminal(START_SYMBOL)
        fake_axiom |= start_nonterminal
        self.start = start_nonterminal

        self.nonterms = []
        self.terminals = set()
        self.symbols = ()
        self.productions = []
        self.nonterm_offset = {}
        self.__first_sets = {}

        def register(symbol):
            if isinstance(symbol, BaseTerminal):
                self.terminals.add(symbol)
            else:
                assert(isinstance(symbol, NonTerminal))
                if symbol not in self.nonterms:
                    self.nonterms.append(symbol)

            return symbol

        register(fake_axiom)

        scanned_count = 0
        while scanned_count < len(self.nonterms):
            last_unscanned = len(self.nonterms)

            for nt_idx in range(scanned_count, last_unscanned):
                nt = self.nonterms[nt_idx]
                self.nonterm_offset[nt] = len(self.productions)

                for prod, func, prec in nt.enum_rules():
                    for symbol in prod:
                        register(symbol)
                    self.productions.append((nt, prod, func, prec))

            scanned_count = last_unscanned

        self.terminals = tuple(sorted(self.terminals, key=id))
        self.nonterms = tuple(sorted(self.nonterms, key=lambda nt: nt.name))
        self.symbols = self.nonterms + self.terminals
        self.skipped_domains = []

        self.__build_first_sets()
        self.table = ParsingTable(self)

        self.ll1_table = None
        self.ll1_is_ok = True

    def first_set(self, x):
        result = set()
        skippable_symbols = 0
        for sym in x:
            fs = self.__first_sets.get(sym, {sym})
            result.update(fs - {None})
            if None in fs:
                skippable_symbols += 1
            else:
                break
        if skippable_symbols == len(x):
            result.add(None)
        return frozenset(result)

    def __build_first_sets(self):
        for s in self.nonterms:
            self.__first_sets[s] = set()
            if [] in s.productions:
                self.__first_sets[s].add(None)

        repeat = True
        while repeat:
            repeat = False

            for nt, prod, _, _ in self.productions:
                curfs = self.__first_sets[nt]
                curfs_len = len(curfs)
                curfs.update(self.first_set(prod))

                if len(curfs) > curfs_len:
                    repeat = True

        self.__first_sets = {x: frozenset(y) for x, y in self.__first_sets.items()}

    def stringify(self, indexes=True):
        lines = '\n'.join(nt.stringify() for nt in self.nonterms)
        if indexes:
            lines = '\n'.join(RULE_INDEXING_PATTERN % (x, y)
                              for x, y in enumerate(lines.split('\n')))
        return lines

    def __str__(self):
        return self.stringify()

    def add_skipped_domain(self, regex):
        self.skipped_domains.append(regex)

    def parse(self, text):
        lexer = Lexer(self.terminals, text, self.skipped_domains)
        stack = [(0, Fragment(Position(), Position()), None)]
        try:
            cur = lexer.next_token()
        except LexerError as lex_err:
            raise ParseError(pos=lex_err.pos, unexpected=lex_err, expected=[], _text=lex_err.message) from lex_err

        while True:
            cur_state, cur_coord, top_attr = stack[-1]
            actions = self.table.action[cur_state][cur.type]
            if len(actions) < 2:
                action = next(iter(actions), None)
            elif len(actions) == 2:
                # Shift/reduce conflict: resolve via precedence
                shift_action = None
                reduce_action = None
                for act in actions:
                    if isinstance(act, Shift):
                        shift_action = act
                    elif isinstance(act, Reduce):
                        reduce_action = act
                if shift_action is not None and reduce_action is not None:
                    _, _, fold, prod_prec = self.productions[reduce_action.rule]
                    if prod_prec is None:
                        prod = self.productions[reduce_action.rule][1]
                        for sym in reversed(prod):
                            if isinstance(sym, BaseTerminal):
                                prod_prec = Precedence(sym.priority, 'left')
                                break
                    token_prec = cur.type.priority
                    if prod_prec is None:
                        action = shift_action
                    elif token_prec > prod_prec.level:
                        action = shift_action
                    elif token_prec < prod_prec.level:
                        action = reduce_action
                    else:
                        if prod_prec.associativity == 'left':
                            action = reduce_action
                        elif prod_prec.associativity == 'right':
                            action = shift_action
                        else:
                            raise ParseError(pos=cur.pos.start, unexpected=cur, expected=[cur.type], _text="Неассоциативная операция")
                else:
                    action = shift_action or reduce_action

            match action:
                case Shift(state):
                    stack.append((state, cur.pos, cur.attr))
                    try:
                        cur = lexer.next_token()
                    except LexerError as lex_err:
                        raise ParseError(pos=lex_err.pos, unexpected=lex_err, expected=[], _text=lex_err.message) from lex_err

                case Reduce(rule):
                    nt, prod, fold, _ = self.productions[rule]
                    n = len(prod)
                    attrs = [attr for state, coord, attr in stack[len(stack)-n:]
                             if attr != None]
                    coords = [coord for state, coord, attr in stack[len(stack)-n:]]
                    if len(coords) > 0:
                        res_coord = Fragment(coords[0].start, coords[-1].following)
                    else:
                        res_coord = Fragment(cur.pos.start, cur.pos.start)
                    del stack[len(stack)-n:]
                    goto_state = self.table.goto[stack[-1][0]][nt]
                    res_attr = fold.callee(attrs, coords, res_coord)
                    stack.append((goto_state, res_coord, res_attr))
                case Accept():
                    assert(len(stack) == 2)
                    return top_attr
                case None:
                    expected = [symbol for symbol, actions
                                in self.table.action[cur_state].items()
                                if len(actions) > 0]
                    raise ParseError(pos=cur.pos.start, unexpected=cur,
                                     expected=expected)

    def parse_earley(self, text):
        tokens = list(self.tokenize(text))
        tokens = [token for token in tokens if token.type != EOF_SYMBOL]
        earley_parser = EarleyParser(self)
        res = earley_parser.parse(tokens)

        return res

    def parse_ll1(self, text):
        if not self.is_ll1():
            raise ValueError("Grammar is not LL(1); cannot parse in LL(1) mode.")

        lexer = Lexer(self.terminals, text, self.skipped_domains)

        start_nt = self.nonterms[0]
        root = ParseTreeNode(symbol=start_nt, fold=None)

        stack = [ParseTreeNode(symbol=EOF_SYMBOL), root]

        cur_token = lexer.next_token()

        while True:
            top_node = stack[-1]
            top_sym = top_node.symbol

            if isinstance(top_sym, BaseTerminal):
                if top_sym == cur_token.type:
                    top_node.token = cur_token
                    stack.pop()
                    if top_sym == EOF_SYMBOL:
                        break
                    cur_token = lexer.next_token()
                else:
                    expected = [top_sym]
                    raise ParseError(pos=cur_token.pos.start,
                                     unexpected=cur_token,
                                     expected=expected)
            else:
                table_row = self.ll1_table.table[top_sym]
                entry = table_row.get(cur_token.type, None)
                if entry is None:
                    expected = list(table_row.keys())
                    raise ParseError(pos=cur_token.pos.start,
                                     unexpected=cur_token,
                                     expected=expected)

                stack.pop()
                prod_symbols, fold = entry
                children_nodes = []
                for sym in prod_symbols:
                    child_node = ParseTreeNode(symbol=sym)
                    children_nodes.append(child_node)
                top_node.children = children_nodes
                top_node.fold = fold

                for child in reversed(children_nodes):
                    stack.append(child)

        # print(root)
        self._evaluate_parse_tree(root)
        return root.attribute

    def build_ll1_table(self):
        if self.ll1_table is not None:
            return
        try:
            self.ll1_table = PredictiveParsingTable(self)
        except PredictiveTableConflictError:
            self.ll1_is_ok = False
            raise

    def is_ll1(self):
        if self.ll1_table is None and self.ll1_is_ok:
            try:
                self.build_ll1_table()
            except PredictiveTableConflictError:
                return False
        return self.ll1_is_ok

    def stringify_ll1_table(self):
        if not self.is_ll1():
            return "Grammar is NOT LL(1) - conflicts found."
        return self.ll1_table.stringify()

    def _evaluate_parse_tree(self, node: ParseTreeNode):
        if isinstance(node.symbol, BaseTerminal):
            node.attribute = node.token.attr if node.token else None
            return node.attribute

        for child in node.children:
            self._evaluate_parse_tree(child)
        attrs = [child.attribute for child in node.children if child.attribute is not None]

        coords = [child.token.pos for child in node.children if child.token is not None]
        if len(coords) > 0:
            res_coord = Fragment(coords[0].start, coords[-1].following)
        else:
            if node.children:
                coords_for_start = [c for c in node.children if c.token is not None]
                if coords_for_start:
                    start = coords_for_start[0].start
                else:
                    start = Position()
            else:
                start = Position()
            res_coord = Fragment(start, start)

        if node.fold is not None:
            node.attribute = node.fold.callee(attrs, coords, res_coord)
        else:
            if len(attrs) == 1:
                node.attribute = attrs[0]
            elif len(attrs) == 0:
                node.attribute = None
            else:
                raise RuntimeError('No fold function for production with multiple children!')
        return node.attribute

    def tokenize(self, text):
        lexer = Lexer(self.terminals, text, self.skipped_domains)

        while True:
            token = lexer.next_token()
            yield token
            if token.type == EOF_SYMBOL:
                break

    def is_lalr_one(self):
        return self.table.is_lalr_one()

    def print_table(self, file=sys.stdout):
        print(self.table.stringify(), file=file)


def goto(gr, item_set, inp):
    result_set = set()
    for (item, lookahead) in item_set:
        prod_id, dot = item
        _, pbody, _, _ = gr.productions[prod_id]
        if dot == len(pbody) or pbody[dot] != inp:
            continue

        new_item = ((prod_id, dot + 1), lookahead)
        result_set.add(new_item)

    result_set = closure(gr, result_set)
    return result_set


def kernels(item_set):
    return frozenset((item, nextsym) for item, nextsym in item_set if item[1] > 0 or item[0] == 0)


def drop_itemset_lookaheads(itemset):
    return frozenset((x[0], x[1]) for x, y in itemset)


def describe_grammar(gr):
    return '\n'.join([
        'Grammar rules (%d in total):' % len(gr.productions),
        str(gr) + '\n',
        'Grammar non-terminals (%d in total):' % len(gr.nonterms),
        '\n'.join('\t' + str(s) for s in gr.nonterms) + '\n',
        'Grammar terminals (%d in total):' % len(gr.terminals),
        '\n'.join('\t' + str(s) for s in gr.terminals)
    ])


def describe_parsing_table(table):
    conflict_status = table.get_conflict_status()

    def conflict_status_str(state_id):
        has_sr_conflict = (conflict_status[state_id] == STATUS_SR_CONFLICT)
        status_str = ('shift-reduce' if has_sr_conflict else 'reduce-reduce')
        return 'State %d has a %s conflict' % (state_id, status_str)

    return ''.join([
        'PARSING TABLE SUMMARY\n',
        'Is the given grammar LALR(1)? %s\n' % ('Yes' if table.is_lalr_one() else 'No'),
        ''.join(conflict_status_str(sid) + '\n' for sid in range(table.n_states)
                if conflict_status[sid] != STATUS_OK) + '\n',
        table.stringify()
    ])


RULE_INDEXING_PATTERN = '%-5d%s'
START_SYMBOL = '$accept'

STATUS_OK = 0
STATUS_SR_CONFLICT = 1
STATUS_RR_CONFLICT = 2


class LR0_Automaton:
    def __init__(self, gr):
        self.states = []
        self.id_from_state = dict()
        self.goto = dict()

        self.states = [LR0_Automaton.__closure(gr, [(0, 0)])]
        next_id = 0

        self.id_from_state[self.states[-1]] = next_id
        next_id += 1

        seen = set(self.states)
        set_queue = self.states
        while len(set_queue) > 0:
            new_elements = []
            for item_set in set_queue:
                item_set_id = self.id_from_state[item_set]
                for symbol in gr.symbols:
                    next_item_set = LR0_Automaton.__goto(gr, item_set, symbol)
                    if len(next_item_set) == 0:
                        continue
                    if next_item_set not in seen:
                        new_elements += [next_item_set]
                        seen.add(next_item_set)
                        self.states += [next_item_set]
                        self.id_from_state[self.states[-1]] = next_id
                        next_id += 1
                    self.goto[(item_set_id, symbol)] = self.id_from_state[next_item_set]
            set_queue = new_elements

    @staticmethod
    def __closure(gr, item_set):
        result = set(item_set)
        set_queue = item_set
        while len(set_queue) > 0:
            new_elements = []
            for itemProdId, dot in set_queue:
                _, pbody, _, _ = gr.productions[itemProdId]
                if dot == len(pbody) or pbody[dot] not in gr.nonterms:
                    continue
                nt = pbody[dot]
                nt_offset = gr.nonterm_offset[nt]
                for idx in range(len(nt.productions)):
                    new_item_set = (nt_offset + idx, 0)
                    if new_item_set not in result:
                        new_elements += [new_item_set]
                        result.add(new_item_set)
            set_queue = new_elements
        return frozenset(result)


    @staticmethod
    def __goto(gr, item_set, inp):
        result_set = set()
        for prod_index, dot in item_set:
            _, pbody, _, _ = gr.productions[prod_index]
            if dot < len(pbody) and pbody[dot] == inp:
                result_set.add((prod_index, dot + 1))
        result_set = LR0_Automaton.__closure(gr, result_set)
        return result_set


    @staticmethod
    def __kernels(item_set):
        return frozenset((x, y) for x, y in item_set if y > 0 or x == 0)

    def kstates(self):
        return [LR0_Automaton.__kernels(st) for st in self.states]


class LexerError(Error):
    ERROR_SLICE = 10

    def __init__(self, pos, text, message=""):
        self.pos = pos
        self.bad = text[pos.offset:pos.offset + self.ERROR_SLICE]
        self._message = f'Не удалось разобрать {self.bad!r}' if message == "" else message

    def __repr__(self):
        return f'LexerError({self.pos!r},{self.bad!r})'

    @property
    def message(self):
        return self._message


class Lexer:
    def __init__(self, domains, text, skip):
        self.domains = list(domains)
        self.text = text
        self.pos = Position()
        self.skip_token = object()
        self.domains += [Terminal('-skip-', regex, lambda _: self.skip_token)
                         for regex in skip]
        self.domains.append(ErrorTerminal())

    def next_token(self):
        while self.pos.offset < len(self.text):
            offset = self.pos.offset
            matches = [(d, d.priority, *d.match(self.text, offset))
                       for d in self.domains]
            domain, priority, length, attr = \
                    max(matches, key=lambda t: (t[2], t[1]))

            assert length > 0

            if attr == ErrorTerminal:
                raise LexerError(self.pos, self.text)

            new_pos = self.pos.shift(self.text[offset:offset + length])
            frag = Fragment(self.pos, new_pos)
            self.pos = new_pos
            if attr != self.skip_token:
                token = Token(domain, frag, attr)
                return token

        return Token(EOF_SYMBOL, Fragment(self.pos, self.pos), None)

@dataclasses.dataclass(frozen=True)
class EarleyState:
    rule: tuple
    dot: int
    start: int
    end: int
    attrs: any = dataclasses.field(default_factory=lambda: None, hash=False)
    coords: tuple = ()

    def __post_init__(self):
        if type(self.coords) is not tuple:
            object.__setattr__(self, 'coords', tuple(self.coords))

    def __repr__(self):
        lhs, rhs, _ = self.rule
        dotted_rhs = ' '.join(str(x) for x in rhs[:self.dot]) + ' • ' + ' '.join(str(x) for x in rhs[self.dot:])
        return f"{lhs} → {dotted_rhs} [{self.start}, {self.end}] {self.is_complete()} attr({self.attrs})"

    def is_complete(self):
        _, rhs, _ = self.rule
        return self.dot == len(rhs)

    def next_symbol(self):
        _, rhs, _ = self.rule
        if self.dot < len(rhs):
            return rhs[self.dot]
        return None


class EarleyParser:
    def __init__(self, grammar: Parser):
        self.grammar = grammar
        self.chart = collections.defaultdict(set)

    def predict(self, state, pos, coords, states):
        if not isinstance(coords, tuple):
            coords = (coords,)
        next_sym = state.next_symbol()
        if isinstance(next_sym, NonTerminal):
            for prod, fold, _ in next_sym.enum_rules():
                new_state = EarleyState((next_sym, tuple(prod), fold),
                                        0,
                                        pos,
                                        pos,
                                        attrs=[],
                                        coords=coords)
                if new_state not in self.chart[pos] and new_state not in states:
                    states.append(new_state)
                    self.predict(new_state, pos, coords, states)

    def scan(self, state, token, pos):
        next_sym = state.next_symbol()
        if (isinstance(next_sym, LiteralTerminal) or isinstance(next_sym, Terminal)) and next_sym == token.type:
            new_attrs = []

            if token.attr is None:
                new_attrs = state.attrs
            elif not isinstance(state, list) or len(token.attr) > 1:
                new_attrs = state.attrs + [token.attr]
            else:
                new_attrs = state.attrs + token.attr

            new_coords = state.coords + (token.pos,)
            new_state = EarleyState(state.rule,
                                    state.dot + 1,
                                    state.start,
                                    pos + 1,
                                    new_attrs,
                                    new_coords)
            if new_state.is_complete():
                _, _, fold = new_state.rule
                coords = new_state.coords
                res_coord = Fragment(coords[0].start, coords[-1].following)
                res_attr = fold.callee(new_attrs, coords, res_coord)

                new_state = EarleyState(state.rule, state.dot + 1, state.start, pos + 1, [res_attr], new_coords)

            self.chart[pos + 1].add(new_state)

    def complete(self, state: EarleyState, pos, states: list[EarleyState]):
        for prev_state in self.chart[state.start]:
            next_sym = prev_state.next_symbol()
            if next_sym == state.rule[0]:
                state_attrs = state.attrs
                new_attrs = []

                if state.is_complete() and state.start == state.end:
                    state_attrs = [state_attrs]

                if prev_state.attrs is None:
                    new_attrs = state_attrs
                elif not isinstance(state_attrs, list) or len(state_attrs) > 1 or state.is_complete():
                    new_attrs = prev_state.attrs + state_attrs
                else:
                    new_attrs = prev_state.attrs + state_attrs

                new_coords = prev_state.coords + state.coords
                new_state = EarleyState(
                    prev_state.rule,
                    prev_state.dot + 1,
                    prev_state.start,
                    pos,
                    new_attrs,
                    new_coords
                )
                if new_state not in self.chart[pos]:
                    states.append(new_state)
                    if not new_state.is_complete():
                        self.predict(new_state, pos, new_coords, states)
                if new_state.is_complete():
                    _, _, fold = new_state.rule
                    # if new_state.attrs is not None:
                        # if isinstance(new_state.attrs, list):
                        #     attrs = [attr for attr in list(new_state.attrs) if attr]
                        # else:
                        #     attrs = [new_state.attrs]
                    attrs = new_state.attrs
                    coords = new_state.coords
                    res_coord = Fragment(coords[0].start, coords[-1].following)

                    res_attr = []

                    res_attr = fold.callee(attrs, coords, res_coord)
                    states.remove(new_state)
                    new_state = dataclasses.replace(new_state, attrs=[res_attr], coords=[res_coord])
                    states.append(new_state)


    def parse(self, tokens):
        start_rule = (self.grammar.nonterms[0], tuple(self.grammar.productions[0][1]), self.grammar.productions[0][2])
        self.chart[0].add(EarleyState(start_rule, 0, 0, 0))

        for pos in range(len(tokens)+1):
            states = list(self.chart[pos])

            if len(states) == 0:
                expected_set = set()
                last_chart = self.chart[pos-1] if pos > 0 else self.chart[0]
                for state in last_chart:
                    next_sym = state.next_symbol()
                    if next_sym is not None:
                        if isinstance(next_sym, Terminal):
                            expected_set.add(next_sym)
                        elif isinstance(next_sym, NonTerminal):
                            expected_set.update(self.grammar.first_set([next_sym]) - {None})
                expected_list = list(expected_set)
                raise ParseError(tokens[pos-1].pos.start,
                                 unexpected=tokens[pos-1],
                                 expected=expected_list)

            i = 0
            while i  < len(states):
                state = states[i]
                if not state.is_complete():
                    next_sym = state.next_symbol()
                    # print(next_sym)
                    if isinstance(next_sym, NonTerminal):
                        # print('a', next_sym)
                        self.predict(state, pos, tokens[pos].pos, states)
                    elif pos < len(tokens):
                        self.scan(state, tokens[pos], pos)
                else:
                    self.complete(state, pos, states)
                i+=1
                self.chart[pos].update(states)

        final_states = [state
                        for state in self.chart[len(tokens)]
                        if (state.rule[0] == self.grammar.start
                            and state.is_complete()
                            and state.start == 0)]
        if len(final_states) > 1:
            raise ParseError(pos=Position(),
                             expected="",
                             unexpected="",
                             _text=f"Неопределенная грамматика: найдено {len(final_states)} путей разбора")
        if final_states:
            return final_states[0].attrs[0]

    def print_chart(self):
        for pos, states in sorted(self.chart.items()):
            print(f"Chart[{pos}]:")
            for state in states:
                print(f"  {state}")
