module Parser (parse) where

import Lexer (Token(..), tokenize)
import AST (Program, Articles, Body, Instr(..))

-- | Тип парсера: принимает список токенов и возвращает либо ошибку,
--   либо пару (результат, оставшиеся токены).
type Parser a = [Token] -> Either String (a, [Token])

-- | Список зарезервированных слов, которые не могут встречаться как простые инструкции.
reservedWords :: [String]
reservedWords = ["endif", "else", "end", "define"]

-- | Проверка, является ли текущий токен стоп-токеном, указанным в списке.
isStop :: [String] -> Token -> Bool
isStop stops (TokWord w) = w `elem` stops
isStop _ _ = False

-- | Проверка, что токен является началом конструкции if.
isIf :: Token -> Bool
isIf (TokWord "if") = True
isIf _ = False

-- | Разбор простого токена: число или слово.
-- Если встречается зарезервированное слово (например, "endif") вне ожидаемого контекста,
-- то разбора завершается с ошибкой.
parseSimple :: Parser Instr
parseSimple [] = Left "Unexpected end of input in parseSimple"
parseSimple (tok:ts) =
  case tok of
    TokNum n -> Right (INum n, ts)
    TokWord w ->
      if w `elem` reservedWords
         then Left ("Unexpected reserved word: " ++ w)
         else Right (IWord w, ts)

-- | Разбор конструкции if:
-- <if> ::= "if" <Body> (<ElsePart>) "endif"
-- Если присутствует ветка else, она оборачивается в Just.
parseIf :: Parser Instr
parseIf (TokWord "if" : tokens) = do
  (thenBody, tokens1) <- parseBody ["else", "endif"] tokens
  case tokens1 of
    (TokWord "else" : tokens2) -> do
       (elseBody, tokens3) <- parseBody ["endif"] tokens2
       case tokens3 of
         (TokWord "endif" : tokens4) -> Right (IIf thenBody (Just elseBody), tokens4)
         _ -> Left "Expected 'endif' after else branch"
    (TokWord "endif" : tokens2) ->
       Right (IIf thenBody Nothing, tokens2)
    _ -> Left "Expected 'else' or 'endif' after if body"
parseIf _ = Left "Expected 'if'"

-- | Разбор тела (Body): читаем инструкции до появления стоп-токена.
parseBody :: [String] -> Parser Body
parseBody stops tokens =
  case tokens of
    [] -> Right ([], [])
    (t:ts) ->
      if isStop stops t then Right ([], tokens)
      else if isIf t then do
             (instr, rest) <- parseIf (t:ts)
             (instrs, rest') <- parseBody stops rest
             Right (instr : instrs, rest')
           else do
             (instr, rest) <- parseSimple (t:ts)
             (instrs, rest') <- parseBody stops rest
             Right (instr : instrs, rest')

-- | Разбор статей: пока следующий токен – "define", парсим статью.
parseArticles :: Parser Articles
parseArticles tokens =
  case tokens of
    (TokWord "define" : _) -> do
      (article, rest) <- parseArticle tokens
      (articles, rest') <- parseArticles rest
      Right (article : articles, rest')
    _ -> Right ([], tokens)

-- | Разбор одной статьи:
-- <Article> ::= "define" word <Body> "end"
parseArticle :: Parser (String, Body)
parseArticle (TokWord "define" : tokens) = do
  case tokens of
    [] -> Left "Unexpected end of input after 'define'"
    (TokWord name : tokens1) -> do
      (body, tokens2) <- parseBody ["end"] tokens1
      case tokens2 of
        (TokWord "end" : tokens3) -> Right ((name, body), tokens3)
        _ -> Left "Expected 'end' at the end of article definition"
    (TokNum _ : _) -> Left "Article name must be a word"
parseArticle _ = Left "Expected 'define' to start article definition"

-- | Разбор всей программы: статьи, затем основное тело.
parseProgram :: Parser Program
parseProgram tokens = do
  (articles, tokens1) <- parseArticles tokens
  (body, tokens2) <- parseBody [] tokens1
  if null tokens2
    then Right ((articles, body), tokens2)
    else Left "Extra tokens after end of program"

-- | Основная функция разбора: возвращает Nothing при синтаксической ошибке.
parse :: String -> Maybe Program
parse input =
  case parseProgram (tokenize input) of
    Right (prog, []) -> Just prog
    _                -> Nothing
