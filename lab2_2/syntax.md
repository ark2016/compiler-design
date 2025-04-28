Входной язык описывает:
*   **Имя класса:** Имя генерируемого класса парсера.
*   **Токены:** Список имен терминальных символов.
*   **Типы:** Сопоставление имен символов (терминалов/нетерминалов) с типами данных (для атрибутов).
*   **Методы:** Сигнатуры методов, которые будут использоваться как семантические действия в грамматике (имя, возвращаемый тип, типы параметров, включая массивы).
*   **Грамматику:** Набор правил вида `Нетерминал = Альтернатива1 | ... | АльтернативаN ;`.
    *   Каждая альтернатива - это последовательность элементов.
    *   Элементы могут быть:
        *   Именем терминала или нетерминала.
        *   Вложенным правилом в скобках `(...)`, которое само состоит из альтернатив.
        *   Повторением элемента, указанным через `%rep Элемент`.
    *   Альтернатива может заканчиваться указанием метода семантического действия (`/ имя_метода`).
*   **Аксиому:** Имя стартового нетерминала грамматики.
Весь файл спецификации представляет собой последовательность этих секций, завершающуюся `%end`.

---

Определим терминальные символы:
*   **Ключевые слова:**
    *   `KW_CLASS = %class`
    *   `KW_TOKENS = %tokens`
    *   `KW_TYPES = %types`
    *   `KW_METHODS = %methods`
    *   `KW_GRAMMAR = %grammar`
    *   `KW_AXIOM = %axiom`
    *   `KW_END = %end`
    *   `KW_REP = %rep`
*   **Идентификатор:** (Имена символов, типов, методов, класса)
    *   `ID = [A-Za-z][A-Za-z0-9_]*` (Начинается с буквы, далее буквы, цифры, подчеркивание - как в примере)
*   **Знаки пунктуации:**
    *   `COLON = :`
    *   `SEMICOLON = ;`
    *   `LPAREN = (`
    *   `RPAREN = )`
    *   `LBRACKET = [`
    *   `RBRACKET = ]`
    *   `COMMA = ,`
    *   `SLASH = /`
    *   `PIPE = |`
    *   `ASSIGN = =`
*   **Пропускаемые токены:**
    *   `WHITESPACE = \s+` (Пробелы, табы, переводы строк)
    *   `COMMENT = \$.*` (Комментарии от `$` до конца строки)



```

NSpec -> NClassSection NTokensSection NTypesSection NMethodsSection NGrammarSection NAxiomSection KW_END

NClassSection   -> KW_CLASS ID 
NTokensSection  -> KW_TOKENS NIdList 
NTypesSection   -> KW_TYPES NTypeMappingList SEMICOLON
NMethodsSection -> KW_METHODS NMethodSignatureList SEMICOLON
NGrammarSection -> KW_GRAMMAR NGrammarRuleList
NAxiomSection   -> KW_AXIOM ID SEMICOLON

// Списки
NIdList -> ID | NIdList COMMA ID
NTypeMappingList -> ε | NTypeMappingList NTypeMapping
NMethodSignatureList -> ε | NMethodSignatureList NMethodSignature
NGrammarRuleList -> ε | NGrammarRuleList NGrammarRule  

// Элементы секций
NTypeMapping -> NIdList COLON NTypeName SEMICOLON
NTypeName    -> ID | ID LBRACKET RBRACKET

NMethodSignature -> NTypeName ID LPAREN NParamListOpt RPAREN SEMICOLON
NParamListOpt    -> ε | NParamList
NParamList       -> NTypeName | NParamList COMMA NTypeName

NGrammarRule -> ID ASSIGN NAlternatives SEMICOLON
NAlternatives -> NRuleAlternative | NAlternatives PIPE NRuleAlternative

NRuleAlternative -> NRuleElementList NSemanticActionOpt
NRuleElementList -> ε | NRuleElementList NRuleElement
NSemanticActionOpt -> ε | SLASH ID

NRuleElement -> ID
             | LPAREN NAlternatives RPAREN  // Вложенное правило
             | KW_REP NRuleElement          // Повторение

```

---

```
// Используется EBNF-подобная нотация:
// * = 0 или более повторений
// + = 1 или более повторений
// ? = 0 или 1 повторение (опционально)
// ( ... ) = группировка

// Общая структура файла спецификации
NSpec = NClassSection NTokensSection NTypesSection NMethodsSection NGrammarSection NAxiomSection KW_END;

// Секции
NClassSection   = KW_CLASS ID;
NTokensSection  = KW_TOKENS NSpaceSeparatedIdList;    
// Использует итеративный список ниже
NTypesSection   = KW_TYPES NTypeMapping*;             
// ИЗМЕНЕНО: Итерация для списка отображений типов (было NTypeMappingList)
NMethodsSection = KW_METHODS NMethodSignature*;       
// ИЗМЕНЕНО: Итерация для списка сигнатур методов (было NMethodSignatureList)
NGrammarSection = KW_GRAMMAR NGrammarRule*;            
// ИЗМЕНЕНО: Итерация для списка правил грамматики (было NGrammarRuleList)
NAxiomSection   = KW_AXIOM ID SEMICOLON;

// --- Определения списков и элементов (с итерацией вместо рекурсии) ---

// Список ID, разделенных пробелами (для секции %tokens)
NSpaceSeparatedIdList = ID+; 
// ИЗМЕНЕНО: 1 или более ID (раньше была левая рекурсия)

// Список ID, разделенных запятыми (для секции %types)
NCommaSeparatedIdList = ID (COMMA ID)*; 
// ИЗМЕНЕНО: 1 ID, за которым 0 или более (COMMA ID) (раньше была левая рекурсия)

// Элемент секции Types
NTypeMapping = NCommaSeparatedIdList COLON NTypeName SEMICOLON; 
// Использует NCommaSeparatedIdList
NTypeName    = ID (LBRACKET RBRACKET)?; 
// ИЗМЕНЕНО: Опциональные скобки []

// Элемент секции Methods
NMethodSignature = NTypeName ID LPAREN NParamList? RPAREN SEMICOLON; // ИЗМЕНЕНО: NParamList сделан опциональным (заменяет NParamListOpt)
NParamList       = NTypeName (COMMA NTypeName)*; // ИЗМЕНЕНО: Список параметров через запятую (раньше была левая рекурсия)

// Элемент секции Grammar
NGrammarRule  = ID ASSIGN NAlternatives SEMICOLON; 
// Использует NAlternatives
NAlternatives = NRuleAlternative (PIPE NRuleAlternative)*; 
// ИЗМЕНЕНО: Список альтернатив через | (раньше была левая рекурсия)

NRuleAlternative = NRuleElement* (SLASH ID)?; 
// ИЗМЕНЕНО: 0 или более элементов правила, за которыми опциональное действие (заменяет NRuleElementList и NSemanticActionOpt)

NRuleElement = ID
             | LPAREN NAlternatives RPAREN // Вложенное правило
             | KW_REP NRuleElement;      // Повторение (остается рекурсивным, но это не левая рекурсия списка)

// Удалены нетерминалы, замененные итерацией или опциональностью:
// NTypeMappingList -> заменен на NTypeMapping* в NTypesSection
// NMethodSignatureList -> заменен на NMethodSignature* в NMethodsSection
// NGrammarRuleList -> заменен на NGrammarRule* в NGrammarSection
// NParamListOpt -> заменен на NParamList? в NMethodSignature
// NRuleElementList -> заменен на NRuleElement* в NRuleAlternative
// NSemanticActionOpt -> заменен на (SLASH ID)? в NRuleAlternative
```
