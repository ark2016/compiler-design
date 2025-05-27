module Lexer where

import Data.Char (isAlpha, isDigit, isSpace)
import AST (Located(..), ParseError(..))

-- Токены лексического анализатора
data Token = 
    -- Ключевые слова
    TypeKW | RecordKW | EndKW | PointerKW | ToKW | VarKW | BeginKW 
  | WhileKW | DoKW | IfKW | ThenKW | ElseKW | NewKW 
  | RealKW | IntegerKW | NotKW | AndKW | DivKW | ModKW | OrKW
    -- Операторы
  | Plus | Minus | Multiply | Divide | Equal | NotEqual
  | Less | LessEqual | Greater | GreaterEqual
    -- Разделители и специальные символы
  | Comma | Semicolon | Colon | LParen | RParen
  | Caret | Assign | Period
    -- Идентификаторы и литералы
  | Ident String | IntLit Integer | RealLit Double
    -- Конец файла
  | EOF
  deriving (Show, Eq)

-- Состояние лексера
data LexerState = LexerState {
  source :: String,      -- исходный текст
  position :: (Int, Int), -- строка и столбец
  currentChar :: Maybe Char,  -- текущий символ или Nothing, если конец файла
  tokens :: [Located Token]    -- накопленные токены
}

-- Инициализация лексера
initLexer :: String -> LexerState
initLexer src = LexerState {
  source = src,
  position = (1, 1), -- начинаем с первой строки, первого столбца
  currentChar = if null src then Nothing else Just (head src),
  tokens = []
}

-- Перемещение к следующему символу
advance :: LexerState -> LexerState
advance ls@LexerState{source="", currentChar=Nothing} = ls
advance ls@LexerState{source="", currentChar=Just _} = ls {
  currentChar = Nothing
}
advance ls@LexerState{source=s:ss, position=(row, col)} =
  if s == '\n'
  then ls {
    source = ss,
    position = (row + 1, 1),
    currentChar = if null ss then Nothing else Just (head ss)
  }
  else ls {
    source = ss,
    position = (row, col + 1),
    currentChar = if null ss then Nothing else Just (head ss)
  }

-- Предварительная обработка - удаление комментариев из исходного текста
removeComments :: String -> String
removeComments [] = []
removeComments ('(':rest) = 
  case rest of
    '*':moreRest -> removeComments (skipComment moreRest 1)
    _ -> '(' : removeComments rest
  where
    skipComment :: String -> Int -> String 
    skipComment [] _ = []  -- Если достигли конца ввода, комментарий не закрыт
    skipComment ('*':')':rs) 1 = rs
    skipComment ('(':'*':rs) level = skipComment rs (level + 1)
    skipComment ('*':')':rs) level = skipComment rs (level - 1)
    skipComment (c:rs) level = skipComment rs level
removeComments (c:rest) = c : removeComments rest

-- Получение всех токенов из исходного текста
tokenize :: String -> Either ParseError [Located Token]
tokenize input = do
  let processedInput = removeComments input
  let initialState = initLexer processedInput
  case runLexer initialState of
    Left err -> Left err
    Right lexState -> Right (reverse $ tokens lexState)

-- Запуск лексера и получение всех токенов
runLexer :: LexerState -> Either ParseError LexerState
runLexer ls@LexerState{currentChar=Nothing} =
  Right (addToken ls EOF)
runLexer ls =
  case getNextToken ls of
    Left err -> Left err
    Right newState -> runLexer newState

-- Добавление токена в список накопленных токенов
addToken :: LexerState -> Token -> LexerState
addToken ls@LexerState{position=pos, tokens=toks} token =
  ls { tokens = Located pos token : toks }

-- Получение следующего токена
getNextToken :: LexerState -> Either ParseError LexerState
getNextToken ls@LexerState{currentChar=Nothing} = Right (addToken ls EOF)
getNextToken ls@LexerState{currentChar=Just c}
  | isSpace c = getNextToken (skipWhitespace ls)
  | isAlpha c = Right (lexIdentOrKeyword ls)
  | isDigit c = lexNumber ls
  | otherwise = lexSymbol ls

-- Пропуск пробельных символов
skipWhitespace :: LexerState -> LexerState
skipWhitespace ls@LexerState{currentChar=Nothing} = ls
skipWhitespace ls@LexerState{currentChar=Just c}
  | isSpace c = skipWhitespace (advance ls)
  | otherwise = ls

-- Проверка следующего символа без его потребления
peekNext :: LexerState -> Maybe Char
peekNext LexerState{source=""} = Nothing
peekNext LexerState{source=s:_} = Just s

-- Распознавание идентификатора или ключевого слова
lexIdentOrKeyword :: LexerState -> LexerState
lexIdentOrKeyword ls = 
  let (ident, newState) = collectIdent ls ""
      token = case ident of
        "TYPE" -> TypeKW
        "RECORD" -> RecordKW
        "END" -> EndKW
        "POINTER" -> PointerKW
        "TO" -> ToKW
        "VAR" -> VarKW
        "BEGIN" -> BeginKW
        "WHILE" -> WhileKW
        "DO" -> DoKW
        "IF" -> IfKW
        "THEN" -> ThenKW
        "ELSE" -> ElseKW
        "NEW" -> NewKW
        "REAL" -> RealKW
        "INTEGER" -> IntegerKW
        "NOT" -> NotKW
        "AND" -> AndKW
        "DIV" -> DivKW
        "MOD" -> ModKW
        "OR" -> OrKW
        _ -> Ident ident
  in addToken newState token

-- Сбор идентификатора
collectIdent :: LexerState -> String -> (String, LexerState)
collectIdent ls@LexerState{currentChar=Nothing} acc = (reverse acc, ls)
collectIdent ls@LexerState{currentChar=Just c} acc
  | isAlpha c || isDigit c || c == '_' = 
      collectIdent (advance ls) (c:acc)
  | otherwise = (reverse acc, ls)

-- Распознавание числового литерала
lexNumber :: LexerState -> Either ParseError LexerState
lexNumber ls = do
  let (intPart, afterInt) = collectDigits ls ""
  case currentChar afterInt of
    Just '.' -> do
      let afterDot = advance afterInt
      case currentChar afterDot of
        Just d | isDigit d -> do
          let (fracPart, afterFrac) = collectDigits afterDot ""
          let realValue = read (intPart ++ "." ++ fracPart) :: Double
          Right (addToken afterFrac (RealLit realValue))
        _ -> Left (ParseError (position afterDot) "Ожидалась дробная часть после точки")
    _ -> Right (addToken afterInt (IntLit (read intPart)))

-- Сбор цифр
collectDigits :: LexerState -> String -> (String, LexerState)
collectDigits ls@LexerState{currentChar=Nothing} acc = (reverse acc, ls)
collectDigits ls@LexerState{currentChar=Just c} acc
  | isDigit c = collectDigits (advance ls) (c:acc)
  | otherwise = (reverse acc, ls)

-- Распознавание символов и операторов
lexSymbol :: LexerState -> Either ParseError LexerState
lexSymbol ls@LexerState{currentChar=Just c, position=pos} =
  case c of
    '+' -> Right (addToken (advance ls) Plus)
    '-' -> Right (addToken (advance ls) Minus)
    '*' -> Right (addToken (advance ls) Multiply)
    '/' -> Right (addToken (advance ls) Divide)
    '=' -> Right (addToken (advance ls) Equal)
    '#' -> Right (addToken (advance ls) NotEqual)
    '<' -> case peekNext ls of
             Just '=' -> Right (addToken (advance (advance ls)) LessEqual)
             _        -> Right (addToken (advance ls) Less)
    '>' -> case peekNext ls of
             Just '=' -> Right (addToken (advance (advance ls)) GreaterEqual)
             _        -> Right (addToken (advance ls) Greater)
    '.' -> Right (addToken (advance ls) Period)
    ',' -> Right (addToken (advance ls) Comma)
    ';' -> Right (addToken (advance ls) Semicolon)
    ':' -> case peekNext ls of
             Just '=' -> Right (addToken (advance (advance ls)) Assign)
             _        -> Right (addToken (advance ls) Colon)
    '(' -> Right (addToken (advance ls) LParen)
    ')' -> Right (addToken (advance ls) RParen)
    '^' -> Right (addToken (advance ls) Caret)
    _ -> Left (ParseError pos $ "Неожиданный символ: " ++ [c])
lexSymbol _ = error "Невозможное состояние в lexSymbol"

