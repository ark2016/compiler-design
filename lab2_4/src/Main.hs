module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, tails, findIndex)

import AST
import Lexer
import Parser

-- Основная функция программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      fileContent <- readFile filePath
      parseFile fileContent
    ["--debug", filePath] -> do
      fileContent <- readFile filePath
      parseFileWithDebug fileContent
    ["--tokens", filePath] -> do
      fileContent <- readFile filePath
      showTokens fileContent
    ["--recover", filePath] -> do
      fileContent <- readFile filePath
      parseFileWithRecovery fileContent
    _ -> do
      putStrLn "Использование: oberon-parser <имя_файла>"
      putStrLn "             или oberon-parser --debug <имя_файла> для вывода АСД"
      putStrLn "             или oberon-parser --tokens <имя_файла> для вывода токенов"
      putStrLn "             или oberon-parser --recover <имя_файла> для парсинга с восстановлением"
      exitFailure

-- Функция для парсинга файла и вывода результата
parseFile :: String -> IO ()
parseFile input = 
  case tokenize input of
    Left err -> printError err
    Right tokens -> 
      case parseProgram tokens of
        Left err -> printError (head err)
        Right ast -> do
          putStrLn "Синтаксический анализ успешно завершен."
          putStrLn "Абстрактное синтаксическое дерево построено."

-- Функция для парсинга файла с восстановлением при ошибках
parseFileWithRecovery :: String -> IO ()
parseFileWithRecovery input = 
  case tokenize input of
    Left err -> printErrorFatal err
    Right tokens -> 
      case parseProgram tokens of
        Left errors -> do
          putStrLn "Обнаружены ошибки:"
          mapM_ printErrorNonFatal errors
          putStrLn "\nПарсер восстановился после ошибок и продолжил анализ."
        Right ast -> do
          putStrLn "Синтаксический анализ успешно завершен."
          putStrLn "Абстрактное синтаксическое дерево построено."

-- Функция для парсинга файла с выводом АСД
parseFileWithDebug :: String -> IO ()
parseFileWithDebug input = 
  case tokenize input of
    Left err -> printErrorFatal err
    Right tokens -> do
      putStrLn "Токены:"
      mapM_ print tokens
      case parseProgram tokens of
        Left errors -> do
          putStrLn "Обнаружены ошибки:"
          mapM_ printErrorNonFatal errors
        Right ast -> do
          putStrLn "Синтаксический анализ успешно завершен."
          putStrLn "Абстрактное синтаксическое дерево:"
          putStrLn (formatAst (show ast))

-- Функция для форматирования AST
formatAst :: String -> String
formatAst str = formatWithIndent 0 str

-- Функция для форматирования с отступами
formatWithIndent :: Int -> String -> String
formatWithIndent indent [] = []
formatWithIndent indent s@('{':rest) = 
  "{\n" ++ formatWithIndent (indent + 2) rest
formatWithIndent indent s@('}':rest) = 
  makeSpaces (indent - 2) ++ "}\n" ++ formatWithIndent (indent - 2) rest
formatWithIndent indent s@('[':rest) = 
  "[\n" ++ formatWithIndent (indent + 2) rest
formatWithIndent indent s@(']':rest) = 
  makeSpaces (indent - 2) ++ "]\n" ++ formatWithIndent (indent - 2) rest
formatWithIndent indent s@('(':rest) =
  "(\n" ++ formatWithIndent (indent + 2) rest
formatWithIndent indent s@(')':rest) =
  makeSpaces (indent - 2) ++ ")\n" ++ formatWithIndent (indent - 2) rest
formatWithIndent indent s@(',':rest) =
  ",\n" ++ makeSpaces indent ++ formatWithIndent indent (dropWhile isSpace rest)
formatWithIndent indent s
  | " = " `isPrefixOf` s = " = " ++ formatWithIndent indent (drop 3 s)
  | otherwise =
      let 
        (token, rest) = break (\c -> c `elem` "{},[]()") s
        
        -- Улучшенная обработка именованных полей (field = value)
        formattedToken = if " = " `isInfixOf` token
                         then let (field, value) = splitAtSubstring " = " token
                              in field ++ " = " ++ value
                         else token
        
        rest' = if null rest then "" else rest
      in
        if all isSpace token && not (null rest)
        then formatWithIndent indent rest
        else makeSpaces indent ++ formattedToken ++ formatWithIndent indent rest'

-- Вспомогательная функция для создания отступов - переименована, чтобы избежать конфликта
makeSpaces :: Int -> String
makeSpaces n = replicate n ' '

-- Разделить строку по подстроке
splitAtSubstring :: String -> String -> (String, String)
splitAtSubstring sub str = 
  case findSubstring sub str of
    Nothing -> (str, "")
    Just idx -> (take idx str, drop (idx + length sub) str)

-- Найти индекс первого вхождения подстроки
findSubstring :: String -> String -> Maybe Int
findSubstring sub str = findIndex (isPrefixOf sub) (tails str)

-- Функция для отображения только токенов
showTokens :: String -> IO ()
showTokens input = 
  case tokenize input of
    Left err -> printError err
    Right tokens -> do
      putStrLn "Токены:"
      mapM_ (\t -> putStrLn $ show t) tokens

-- Функция для вывода ошибок с завершением программы
printErrorFatal :: ParseError -> IO ()
printErrorFatal err = do
  printErrorMsg err
  exitFailure

-- Функция для вывода ошибок без завершения программы
printErrorNonFatal :: ParseError -> IO ()
printErrorNonFatal = printErrorMsg

-- Функция для вывода сообщения об ошибке
printErrorMsg :: ParseError -> IO ()
printErrorMsg (ParseError (line, col) msg) = do
  putStrLn $ "Ошибка в строке " ++ show line ++ ", столбце " ++ show col ++ ":"
  putStrLn msg

-- Для обратной совместимости со старым кодом
printError :: ParseError -> IO ()
printError = printErrorFatal

-- Функция для создания тестового файла Оберона
createTestFile :: IO ()
createTestFile = writeFile "test.oberon" testProgram

-- Тестовая программа на Обероне
testProgram :: String
testProgram = unlines [
  "TYPE",
  "  Point = RECORD",
  "    x, y : REAL;",
  "  END;",
  "  Shape = RECORD",
  "    center : Point;",
  "    color : INTEGER;",
  "    next : POINTER TO Shape;",
  "  END;",
  "  Circle = RECORD(Shape)",
  "    radius : REAL;",
  "  END;",
  "  Rectangle = RECORD(Shape)",
  "    width, height : REAL;",
  "  END;",
  "",
  "VAR",
  "  p1, p2 : Point;",
  "  s : Shape;",
  "  c : Circle;",
  "  r : Rectangle;",
  "  ps : POINTER TO Shape;",
  "  pc : POINTER TO Circle;",
  "  pr : POINTER TO Rectangle;",
  "BEGIN",
  "  p1.x := 10;",
  "  p1.y := 3.5;",
  "  s.center := p1;",
  "  s.color := 100500;",
  "  c := s;",
  "  c.radius := 7;",
  "  r.center.x := 5.2;",
  "  r.center.y := 2.5;",
  "  r.color := 500100;",
  "  r.width := 4.5;",
  "  r.height := 5.4;",
  "  c := r;",
  "  NEW(pr);",
  "  pr^ := r;",
  "  ps := pr;",
  "  NEW(pc);",
  "  pc^ := c;",
  "  ps.next := pc;",
  "",
  "  (* комментарий *)",
  "  WHILE p1.x * p1.y < 77777 DO",
  "    p1.x := p1.x * 1.5;",
  "    p1.y := p1.y * 2.5;",
  "  END;",
  "",
  "  IF p1.x > pc.radius THEN",
  "    p2 := p1;",
  "    p1 := pc.center;",
  "  ELSE",
  "    p2 := pr.center;",
  "  END;",
  "END."
  ] 

