module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure)

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
    _ -> do
      putStrLn "Использование: oberon-parser <имя_файла>"
      putStrLn "             или oberon-parser --debug <имя_файла> для вывода АСД"
      exitFailure

-- Функция для парсинга файла и вывода результата
parseFile :: String -> IO ()
parseFile input = 
  case tokenize input of
    Left err -> printError err
    Right tokens -> 
      case parseProgram tokens of
        Left err -> printError err
        Right ast -> do
          putStrLn "Синтаксический анализ успешно завершен."
          putStrLn "Абстрактное синтаксическое дерево построено."

-- Функция для парсинга файла с выводом АСД
parseFileWithDebug :: String -> IO ()
parseFileWithDebug input = 
  case tokenize input of
    Left err -> printError err
    Right tokens -> do
      putStrLn "Токены:"
      mapM_ print tokens
      case parseProgram tokens of
        Left err -> printError err
        Right ast -> do
          putStrLn "Синтаксический анализ успешно завершен."
          putStrLn "Абстрактное синтаксическое дерево:"
          print ast

-- Функция для вывода ошибок
printError :: ParseError -> IO ()
printError (ParseError (line, col) msg) = do
  putStrLn $ "Ошибка в строке " ++ show line ++ ", столбце " ++ show col ++ ":"
  putStrLn msg
  exitFailure

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