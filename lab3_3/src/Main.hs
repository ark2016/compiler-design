module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit (exitFailure)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf, tails, findIndex, nub, sortBy)
import Control.Monad (forM_, unless)

import AST
import Lexer
import Parser
import Semantic

-- Основная функция программы
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      fileContent <- readFile filePath
      parseAndAnalyzeFile fileContent
    ["--debug", filePath] -> do
      fileContent <- readFile filePath
      parseAndAnalyzeFileWithDebug fileContent
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
parseAndAnalyzeFile :: String -> IO ()
parseAndAnalyzeFile input = 
  case tokenize input of
    Left err -> printError err
    Right tokens -> 
      case parseProgram tokens of
        Left err -> printError (head err)
        Right ast -> do
          -- Выполняем семантический анализ
          case analyzeProgram ast of
            Left semError -> printSemError semError
            Right () -> do
              putStrLn "Программа корректна."

-- Функция для парсинга файла с выводом AST и семантическим анализом
parseAndAnalyzeFileWithDebug :: String -> IO ()
parseAndAnalyzeFileWithDebug input = 
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
          
          -- Выполняем семантический анализ
          case analyzeProgram ast of
            Left semError -> do
              putStrLn "\nОбнаружена семантическая ошибка:"
              printSemError semError
            Right () -> do
              putStrLn "\nПрограмма корректна."

-- Функция для вывода семантической ошибки
printSemError :: SemError -> IO ()
printSemError err = do
  let (line, col) = pos err
      errorMsg = case err of
        TypeNotDefined _ typeName -> 
          "Тип не определен: " ++ typeName
        VarNotDefined _ varName -> 
          "Переменная не определена: " ++ varName
        FieldNotDefined _ recordType errorFieldName -> 
          "Поле '" ++ errorFieldName ++ "' не определено в типе " ++ recordType
        TypeMismatch _ expected actual -> 
          "Несоответствие типов: ожидался " ++ show expected ++ ", получен " ++ show actual
        IncompatibleTypes _ op leftType rightType -> 
          "Несовместимые типы для операции '" ++ op ++ "': " ++ show leftType ++ " и " ++ show rightType
        NotRecordType _ actualType -> 
          "Ожидался тип запись, получен " ++ actualType
        NotPointerType _ actualType -> 
          "Ожидался указатель, получен " ++ actualType
        NoInheritanceRelation _ sourceType targetType -> 
          "Нет отношения наследования между " ++ sourceType ++ " и " ++ targetType
        CannotDereference _ actualType -> 
          "Невозможно разыменовать тип " ++ actualType
        LogicalExprExpected _ actualExprType -> 
          "Ожидалось логическое выражение, получен тип " ++ show actualExprType
  
  putStrLn $ "Ошибка (" ++ show line ++ "," ++ show col ++ "): " ++ errorMsg

-- Функция для парсинга файла с восстановлением при ошибках
parseFileWithRecovery :: String -> IO ()
parseFileWithRecovery input = 
  case tokenize input of
    Left err -> do
      printErrorMsg err
      putStrLn "Ошибка лексического анализа, восстановление невозможно."
      exitFailure
    Right tokens -> do
      putStrLn "Выполняется анализ с восстановлением после ошибок..."
      case parseProgram tokens of
        Left errors -> do
          putStrLn "Обнаружены следующие ошибки:"
          
          -- Группировка похожих ошибок для более компактного вывода
          let 
            -- Функция группировки ошибок
            groupErrors :: [ParseError] -> [(String, Int)]
            groupErrors errs = 
              let grouped = map (\err@(ParseError _ msg) -> 
                    -- Упрощаем сообщения для группировки
                    if "неожиданный конец файла" `isInfixOf` msg then "неожиданный конец файла"
                    else if "ожидался" `isInfixOf` msg then msg
                    else if "неизвестный тип" `isInfixOf` msg then "неизвестный тип"
                    else msg) errs
                  countOccurrences x xs = length (filter (== x) xs)
                  unique = nub grouped
              in map (\msg -> (msg, countOccurrences msg grouped)) unique
            
            -- Если больше 10 ошибок, группируем по типам
            displayErrors = if length errors > 10
                            then let grouped = groupErrors errors
                                     sortedGroups = sortBy (\(_, c1) (_, c2) -> compare c2 c1) grouped
                                 in take 5 sortedGroups
                            else []
          
          -- Показываем либо индивидуальные ошибки, либо сгруппированные
          if length errors <= 10
            then mapM_ printErrorMsg errors
            else do
              forM_ (take 5 errors) printErrorMsg
              putStrLn "..."
              forM_ displayErrors $ \(msg, count) ->
                putStrLn $ "- " ++ msg ++ " (" ++ show count ++ " раз)"
          
          putStrLn $ "\nВсего обнаружено " ++ show (length errors) ++ " ошибок."
          putStrLn "Парсер восстановился после ошибок и продолжил анализ."
          
          -- Подсчет ошибок по типам для анализа
          let 
            categories = [
              ("отсутствует ;", "ожидался ;"),
              ("отсутствует )", "ожидался )"),
              ("отсутствует (", "ожидался ("),
              ("пропущен токен", "пропущен токен"),
              ("отсутствует END", "ожидался END"),
              ("неожиданный конец файла, отсутствует END", "неожиданный конец файла, отсутствует END"),
              ("отсутствует BEGIN", "ожидался BEGIN"),
              ("незакрытый комментарий", "незакрытый комментарий"),
              ("отсутствует точка", "ожидался ."),
              ("неожиданный конец файла", "неожиданный конец файла"),
              ("неизвестный тип", "неизвестный тип"),
              ("ожидался =", "ожидался ="),
              ("ожидался THEN", "ожидался THEN"),
              ("ожидался DO", "ожидался DO")
              ]
            
            countByCategory :: String -> [ParseError] -> Int
            countByCategory pattern = length . filter (\(ParseError _ msg) -> pattern `isSubstringOf` msg)
            
            -- Проверка, является ли подстрока частью строки
            isSubstringOf :: String -> String -> Bool
            isSubstringOf needle haystack = needle `isInfixOf` haystack
            
            -- Сортировка статистики по убыванию количества
            nonEmptyStats = sortBy (\(_, c1) (_, c2) -> compare c2 c1) $
                             filter (\(_, count) -> count > 0) $ 
                             map (\(name, pattern) -> (name, countByCategory pattern errors)) categories
          
          unless (null nonEmptyStats) $ do
            putStrLn "\nСтатистика по типам ошибок:"
            forM_ nonEmptyStats $ \(cat, count) ->
              putStrLn $ "  - " ++ cat ++ ": " ++ show count ++ " шт."
          
        Right ast -> do
          putStrLn "Синтаксический анализ успешно завершен."
          putStrLn "Абстрактное синтаксическое дерево построено."
          
          -- Выполняем семантический анализ
          case analyzeProgram ast of
            Left semError -> do
              putStrLn "\nОбнаружена семантическая ошибка:"
              printSemError semError
            Right () -> do
              putStrLn "\nПрограмма корректна."

-- Функция для вывода токенов
showTokens :: String -> IO ()
showTokens input = 
  case tokenize input of
    Left err -> printError err
    Right tokens -> do
      putStrLn "Токены:"
      mapM_ print tokens

-- Функция для вывода ошибки и завершения программы
printError :: ParseError -> IO ()
printError err = do
  printErrorMsg err
  exitFailure

-- Функция для вывода ошибки без завершения программы
printErrorNonFatal :: ParseError -> IO ()
printErrorNonFatal = printErrorMsg

-- Функция для вывода сообщения об ошибке
printErrorMsg :: ParseError -> IO ()
printErrorMsg (ParseError (line, col) msg) =
  putStrLn $ "Ошибка (" ++ show line ++ "," ++ show col ++ "): " ++ msg

-- Функция для вывода ошибки и завершения программы
printErrorFatal :: ParseError -> IO ()
printErrorFatal err = do
  printErrorMsg err
  exitFailure

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
  case findIndex (isPrefixOf sub) (tails str) of
    Just i -> let (before, after) = splitAt i str
                  subLen = length sub
              in (before, drop subLen (drop i str))
    Nothing -> (str, "")

