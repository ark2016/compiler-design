```
// Общая структура файла спецификации
NSpec -> NClassSection NTokensSection NTypesSection NMethodsSection NGrammarSection NAxiomSection KW_END

// Секции
NClassSection   -> KW_CLASS ID

NTokensSection  -> KW_TOKENS NSpaceSeparatedIdList_NonEmpty // ID+

NTypesSection   -> KW_TYPES                               // For NTypeMapping* when it's empty
                 | KW_TYPES NTypeMapping_NonEmptyList     // For NTypeMapping* when it's non-empty

NMethodsSection -> KW_METHODS                             // For NMethodSignature* when it's empty
                 | KW_METHODS NMethodSignature_NonEmptyList // For NMethodSignature* when it's non-empty

NGrammarSection -> KW_GRAMMAR                             // For NGrammarRule* when it's empty
                 | KW_GRAMMAR NGrammarRule_NonEmptyList    // For NGrammarRule* when it's non-empty

NAxiomSection   -> KW_AXIOM ID SEMICOLON


// --- Определения списков и элементов (без ε) ---

// NSpaceSeparatedIdList = ID+
NSpaceSeparatedIdList_NonEmpty -> ID
                               | ID NSpaceSeparatedIdList_NonEmpty // Right-recursive for ID+

// NTypeMapping* (handled by NTypesSection alternatives)
// NTypeMapping_NonEmptyList for NTypeMapping+
NTypeMapping_NonEmptyList -> NTypeMapping
                           | NTypeMapping NTypeMapping_NonEmptyList // Right-recursive for NTypeMapping+

// NMethodSignature* (handled by NMethodsSection alternatives)
// NMethodSignature_NonEmptyList for NMethodSignature+
NMethodSignature_NonEmptyList -> NMethodSignature
                               | NMethodSignature NMethodSignature_NonEmptyList // Right-recursive for NMethodSignature+

// NGrammarRule* (handled by NGrammarSection alternatives)
// NGrammarRule_NonEmptyList for NGrammarRule+
NGrammarRule_NonEmptyList -> NGrammarRule
                           | NGrammarRule NGrammarRule_NonEmptyList // Right-recursive for NGrammarRule+


// NCommaSeparatedIdList = ID (COMMA ID)*
NCommaSeparatedIdList_NonEmpty -> ID                             // Base case: single ID
                               | ID N_CommaId_Rest_NonEmpty     // ID followed by (COMMA ID)+
N_CommaId_Rest_NonEmpty        -> COMMA ID                      // One COMMA ID
                               | COMMA ID N_CommaId_Rest_NonEmpty // (COMMA ID)+

// NTypeMapping = NCommaSeparatedIdList COLON NTypeName SEMICOLON;
NTypeMapping -> NCommaSeparatedIdList_NonEmpty COLON NTypeName SEMICOLON

// NTypeName = ID (LBRACKET RBRACKET)?;
NTypeName    -> ID                         // ID
             | ID LBRACKET RBRACKET        // ID[]

// NMethodSignature = NTypeName ID LPAREN NParamList? RPAREN SEMICOLON;
NMethodSignature -> NTypeName ID LPAREN RPAREN SEMICOLON                   // ParamList is absent
                 | NTypeName ID LPAREN NParamList_NonEmpty RPAREN SEMICOLON // ParamList is present

// NParamList = NTypeName (COMMA NTypeName)*;
NParamList_NonEmpty -> NTypeName                                 // Base case: single NTypeName
                    | NTypeName N_CommaTypeName_Rest_NonEmpty    // NTypeName followed by (COMMA NTypeName)+
N_CommaTypeName_Rest_NonEmpty -> COMMA NTypeName                  // One COMMA NTypeName
                              | COMMA NTypeName N_CommaTypeName_Rest_NonEmpty // (COMMA NTypeName)+

// NGrammarRule = ID ASSIGN NAlternatives SEMICOLON;
NGrammarRule  -> ID ASSIGN NAlternatives_NonEmpty SEMICOLON // NAlternatives must be non-empty

// NAlternatives = NRuleAlternative (PIPE NRuleAlternative)*;
NAlternatives_NonEmpty -> NRuleAlternative                             // Base case: single NRuleAlternative
                       | NRuleAlternative N_PipeAlternative_Rest_NonEmpty // NRuleAlternative followed by (PIPE NRuleAlternative)+
N_PipeAlternative_Rest_NonEmpty -> PIPE NRuleAlternative                   // One PIPE NRuleAlternative
                                | PIPE NRuleAlternative N_PipeAlternative_Rest_NonEmpty // (PIPE NRuleAlternative)+

// NRuleAlternative = NRuleElement* (SLASH ID)?;
// An alternative cannot be completely empty. It must have elements or an action.
NRuleAlternative -> NRuleElement_NonEmptyList                       // Elements, no action
                 | NRuleElement_NonEmptyList SLASH ID              // Elements, with action
                 | SLASH ID                                        // No elements, with action

// NRuleElement_NonEmptyList for NRuleElement+
NRuleElement_NonEmptyList -> NRuleElement
                           | NRuleElement NRuleElement_NonEmptyList // Right-recursive for NRuleElement+

// NRuleElement = ID | LPAREN NAlternatives RPAREN | KW_REP NRuleElement;
NRuleElement -> ID
             | LPAREN NAlternatives_NonEmpty RPAREN // NAlternatives inside parens must be non-empty
             | KW_REP NRuleElement
```