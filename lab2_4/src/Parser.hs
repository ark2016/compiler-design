{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

-- | Recursive‑descent parser for the Oberon subset (lab 2.4).
--   All lexer tokens are imported *qualified* (alias @L@) to avoid
--   clashes with identically‑named constructors in the AST.
module Parser where

import           AST                       hiding (RelOp (..), Sign (..), AddOp (..), MulOp (..))
import qualified AST                       as AST
import qualified Lexer                     as L

-------------------------------------------------------------------------------
-- Parser state & helpers -----------------------------------------------------
-------------------------------------------------------------------------------

data ParserState = ParserState
  { tokens       :: [AST.Located L.Token]   -- ^ remaining unconsumed tokens
  , currentToken :: AST.Located L.Token     -- ^ look‑ahead (never empty)
  , errors       :: [AST.ParseError]        -- ^ собранные ошибки парсинга
  }

type ParserResult a = Either AST.ParseError (a, ParserState)

-- Точки синхронизации - токены, до которых можно восстановиться
syncTokens :: [L.Token]
syncTokens = [
  L.Semicolon, L.EndKW, L.BeginKW, L.TypeKW, L.VarKW, 
  L.Period, L.IfKW, L.ThenKW, L.ElseKW, L.WhileKW, L.DoKW
  ]

-- Проверка, является ли токен идентификатором или литералом
isIdentOrLiteral :: L.Token -> Bool
isIdentOrLiteral (L.Ident _) = True
isIdentOrLiteral (L.IntLit _) = True
isIdentOrLiteral (L.RealLit _) = True
isIdentOrLiteral _ = False

-- Проверка, является ли токен синтаксически значимым для восстановления
isSyncToken :: L.Token -> Bool
isSyncToken token = token `elem` syncTokens

initParser :: [AST.Located L.Token] -> ParserState
initParser []       = error "Parser: empty token list"
initParser (t : ts) = ParserState ts t []

current :: ParserState -> L.Token
current = AST.unLoc . currentToken

currentPos :: ParserState -> (Int,Int)
currentPos = AST.pos . currentToken

advance :: ParserState -> ParserState
advance ParserState{tokens = [], currentToken = t, errors = errs}   = ParserState [] t errs
advance ParserState{tokens = t:ts, errors = errs}                   = ParserState ts t errs

match :: L.Token -> ParserState -> Either AST.ParseError ParserState
match expected ps@ParserState{currentToken = AST.Located p actual, errors = errs}
  | expected == actual = Right (advance ps)
  | otherwise = Left $ AST.ParseError p ("ожидался " ++ show expected ++ ", получен " ++ show actual)

-- Новая функция для восстановления с сохранением ошибки
matchWithRecovery :: L.Token -> ParserState -> ParserResult ParserState
matchWithRecovery expected ps@ParserState{currentToken = AST.Located p actual, errors = errs}
  | expected == actual = Right (advance ps, advance ps)
  | otherwise = 
      let err = AST.ParseError p ("ожидался " ++ show expected ++ ", получен " ++ show actual)
          ps' = ps { errors = err : errs }
          recoveredPs = recover ps'
      in Right (ps', recoveredPs)

-- Восстановление после ошибки - пропуск токенов до точки синхронизации или идентификатора/литерала
recover :: ParserState -> ParserState
recover ps
  | isEOF ps = ps  -- Если достигнут конец файла, возвращаем текущее состояние
  | isSyncToken (current ps) || isIdentOrLiteral (current ps) = advance ps  -- Если нашли токен синхронизации, двигаемся дальше
  | otherwise = recover (advance ps)  -- Иначе продвигаемся и продолжаем искать

isEOF :: ParserState -> Bool
isEOF = (== L.EOF) . current

-- Функция для сохранения ошибки и продолжения парсинга
addError :: AST.ParseError -> ParserState -> ParserState
addError err ps = ps { errors = err : errors ps }

-- Функция для получения списка ошибок
getErrors :: ParserState -> [AST.ParseError]
getErrors = reverse . errors

-- Защита от бесконечного цикла при восстановлении
maxRecoveryAttempts :: Int
maxRecoveryAttempts = 100

-- Запуск парсера с восстановлением и защитой от зацикливания
tryParse :: (ParserState -> ParserResult a) -> ParserState -> ParserResult a
tryParse parser ps = tryParseWithCounter parser ps maxRecoveryAttempts

-- Версия tryParse с счетчиком попыток для избегания зацикливания
tryParseWithCounter :: (ParserState -> ParserResult a) -> ParserState -> Int -> ParserResult a
tryParseWithCounter parser ps attempts
  | attempts <= 0 = 
      -- Превышено максимальное количество попыток восстановления
      let err = AST.ParseError (currentPos ps) "превышено количество попыток восстановления"
      in Left err
  | otherwise = case parser ps of
      Right res -> Right res
      Left err -> 
        let ps' = addError err ps
            recoveredPs = recover ps'
        in if posEqual ps recoveredPs
           then -- Не удалось восстановиться, пропускаем токен
                if isEOF ps
                then Left $ AST.ParseError (currentPos ps) "неожиданный конец файла, невозможно продолжить анализ"
                else let newPs = advance ps
                     in tryParseWithCounter parser newPs (attempts - 1)
           else tryParseWithCounter parser recoveredPs (attempts - 1)
  where
    posEqual a b = currentPos a == currentPos b

-- Альтернативная функция для попытки разбора с выбором первого успешного
tryAlternatives :: [ParserState -> ParserResult a] -> ParserState -> ParserResult a
tryAlternatives [] ps = Left $ AST.ParseError (currentPos ps) "ни одна из альтернатив не подходит"
tryAlternatives (p:ps') parserState = case p parserState of
  Right res -> Right res
  Left err -> case ps' of
    [] -> Left err
    _ -> tryAlternatives ps' parserState

-- Опциональный парсер - возвращает Nothing при ошибке
optionalParse :: (ParserState -> ParserResult a) -> ParserState -> ParserResult (Maybe a)
optionalParse parser ps = case parser ps of
  Right (res, ps') -> Right (Just res, ps')
  Left _ -> Right (Nothing, ps)

-------------------------------------------------------------------------------
-- Entry point ----------------------------------------------------------------
-------------------------------------------------------------------------------

parseProgram :: [AST.Located L.Token] -> Either [AST.ParseError] AST.Program
parseProgram [] = Left [AST.ParseError (0, 0) "пустой список токенов"]
parseProgram toks = do
  let ps0 = initParser toks
  case parseProgram' ps0 of
    Left err -> Left [err]
    Right (prog, ps1) -> 
      -- После успешного разбора программы, следующий токен должен быть EOF
      case current ps1 of
        L.EOF -> 
          let errs = errors ps1
          in if null errs
             then Right prog
             else Left errs
        _ -> 
          let err = AST.ParseError (currentPos ps1) "лишние символы после END."
              finalErrors = if null (errors ps1) then [err] else err : errors ps1
          in Left finalErrors

-------------------------------------------------------------------------------
-- PROGRAM := ( TYPE … )? ( VAR … )? BEGIN stmtSeq END . ---------------------
-------------------------------------------------------------------------------

parseProgram' :: ParserState -> ParserResult AST.Program
parseProgram' ps = do
  -- Проверка на EOF для обработки незакрытых комментариев
  if isEOF ps && null (tokens ps)
    then Left $ AST.ParseError (currentPos ps) "файл пуст или содержит только незакрытый комментарий"
    else do
      (types, ps1) <- case current ps of
        L.TypeKW -> tryParse parseTypeDecls (advance ps)
        _        -> pure ([], ps)
      
      (vars , ps2) <- case current ps1 of
        L.VarKW  -> tryParse parseVarDecls (advance ps1)
        _        -> pure ([], ps1)
      
      ps3 <- case current ps2 of
        L.BeginKW -> Right (advance ps2)
        _ -> 
          let err = AST.ParseError (currentPos ps2) "ожидался BEGIN"
              ps' = addError err ps2
          in Right (recover ps')
    
      (body, ps4) <- tryParse parseStmtSeq ps3
      
      -- Обработка случая, когда END отсутствует (достигнут EOF)
      ps5 <- case current ps4 of
        L.EndKW -> Right (advance ps4)
        L.EOF -> 
          let err = AST.ParseError (currentPos ps4) "неожиданный конец файла, отсутствует END"
              ps' = addError err ps4
          in Right ps'
        _ -> 
          let err = AST.ParseError (currentPos ps4) "ожидался END"
              ps' = addError err ps4
          in Right (recover ps')
    
      -- Обработка отсутствия завершающей точки
      ps6 <- case current ps5 of
        L.Period -> Right (advance ps5)
        L.EOF -> 
          let err = AST.ParseError (currentPos ps5) "неожиданный конец файла, отсутствует точка"
              ps' = addError err ps5
          in Right ps'
        _ -> 
          let err = AST.ParseError (currentPos ps5) "ожидался ."
              ps' = addError err ps5
          in Right (recover ps')
    
      pure (AST.Program types vars body, ps6)

-------------------------------------------------------------------------------
-- TYPE declarations ----------------------------------------------------------
-------------------------------------------------------------------------------

parseTypeDecls :: ParserState -> ParserResult [AST.TypeDeclaration]
parseTypeDecls ps
  | current ps `elem` [L.VarKW, L.BeginKW] = pure ([], ps)
  | otherwise = do
      (d, ps1) <- tryParse parseTypeDecl ps
      
      ps2 <- case current ps1 of
        L.Semicolon -> Right (advance ps1)
        _ -> 
          let err = AST.ParseError (currentPos ps1) "ожидался ;"
              ps' = addError err ps1
          in Right (recover ps')
      
      (ds, ps3) <- tryParse parseTypeDecls ps2
      pure (d:ds, ps3)

parseTypeDecl :: ParserState -> ParserResult AST.TypeDeclaration
parseTypeDecl ps = case current ps of
  L.Ident name -> do
    ps1 <- Right (advance ps)
    
    ps2 <- case current ps1 of
      L.Equal -> Right (advance ps1)
      _ -> 
        let err = AST.ParseError (currentPos ps1) "ожидался ="
            ps' = addError err ps1
        in Right (recover ps')
    
    (ty, ps3) <- tryParse parseType ps2
    pure (AST.TypeDeclaration name ty, ps3)
    
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор типа"

-- Type ::= REAL | INTEGER | POINTER TO Type | RECORD … END
parseType :: ParserState -> ParserResult AST.Type
parseType ps = case current ps of
  L.RealKW    -> Right (AST.RealType, advance ps)
  L.IntegerKW -> Right (AST.IntegerType, advance ps)
  L.PointerKW -> do
    ps1 <- Right (advance ps)
    
    ps2 <- case current ps1 of
      L.ToKW -> Right (advance ps1)
      _ -> 
        let err = AST.ParseError (currentPos ps1) "ожидался TO"
            ps' = addError err ps1
        in Right (recover ps')
    
    (base, ps3) <- tryParse parseType ps2
    pure (AST.PointerType base, ps3)
  
  L.RecordKW  -> do
    ps1 <- Right (advance ps)
    
    (parent, ps2) <- case current ps1 of
      L.LParen -> do
        ps' <- Right (advance ps1)
        case current ps' of
          L.Ident base -> do
            ps'' <- Right (advance ps')
            ps''' <- case current ps'' of
              L.RParen -> Right (advance ps'')
              _ -> 
                let err = AST.ParseError (currentPos ps'') "ожидался )"
                    ps'''' = addError err ps''
                in Right (recover ps'''')
            pure (Just base, ps''')
          _ -> 
            let err = AST.ParseError (currentPos ps') "ожидался идентификатор базового типа"
                ps'' = addError err ps'
            in Right (Nothing, recover ps'')
      _ -> pure (Nothing, ps1)
    
    (fields, ps3) <- tryParse parseFieldDecls ps2
    
    ps4 <- case current ps3 of
      L.EndKW -> Right (advance ps3)
      _ -> 
        let err = AST.ParseError (currentPos ps3) "ожидался END"
            ps' = addError err ps3
        in Right (recover ps')
    
    pure (AST.RecordType parent fields, ps4)
  
  L.Ident name -> Right (AST.NamedType name, advance ps)
  _ -> Left $ AST.ParseError (currentPos ps) "неизвестный тип"

parseFieldDecls :: ParserState -> ParserResult [AST.FieldDeclaration]
parseFieldDecls ps
  | current ps == L.EndKW = pure ([], ps)
  | otherwise             = do
      (fd, ps1) <- tryParse parseFieldDecl ps
      
      ps2 <- case current ps1 of
        L.Semicolon -> Right (advance ps1)
        _ -> 
          let err = AST.ParseError (currentPos ps1) "ожидался ;"
              ps' = addError err ps1
          in Right (recover ps')
      
      (fds, ps3) <- tryParse parseFieldDecls ps2
      pure (fd:fds, ps3)

parseFieldDecl :: ParserState -> ParserResult AST.FieldDeclaration
parseFieldDecl ps = do
  (ids, ps1) <- tryParse parseIdentList ps
  
  ps2 <- case current ps1 of
    L.Colon -> Right (advance ps1)
    _ -> 
      let err = AST.ParseError (currentPos ps1) "ожидался :"
          ps' = addError err ps1
      in Right (recover ps')
  
  (ty, ps3) <- tryParse parseType ps2
  pure (AST.FieldDeclaration ids ty, ps3)

parseIdentList :: ParserState -> ParserResult [String]
parseIdentList ps = case current ps of
  L.Ident nm -> do
    ps1 <- Right (advance ps)
    case current ps1 of
      L.Comma -> do
        ps2 <- Right (advance ps1)
        (rest, ps3) <- tryParse parseIdentList ps2
        pure (nm:rest, ps3)
      _ -> pure ([nm], ps1)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор"

-------------------------------------------------------------------------------
-- VAR declarations -----------------------------------------------------------
-------------------------------------------------------------------------------

parseVarDecls :: ParserState -> ParserResult [AST.VarDeclaration]
parseVarDecls ps
  | current ps == L.BeginKW = pure ([], ps)
  | otherwise               = do
      (vd, ps1) <- tryParse parseVarDecl ps
      
      ps2 <- case current ps1 of
        L.Semicolon -> Right (advance ps1)
        _ -> 
          let err = AST.ParseError (currentPos ps1) "ожидался ;"
              ps' = addError err ps1
          in Right (recover ps')
      
      (vds, ps3) <- tryParse parseVarDecls ps2
      pure (vd:vds, ps3)

parseVarDecl :: ParserState -> ParserResult AST.VarDeclaration
parseVarDecl ps = do
  (ids, ps1) <- tryParse parseIdentList ps
  
  ps2 <- case current ps1 of
    L.Colon -> Right (advance ps1)
    _ -> 
      let err = AST.ParseError (currentPos ps1) "ожидался :"
          ps' = addError err ps1
      in Right (recover ps')
  
  (ty, ps3) <- tryParse parseType ps2
  pure (AST.VarDeclaration ids ty, ps3)

-------------------------------------------------------------------------------
-- Statements -----------------------------------------------------------------
-------------------------------------------------------------------------------

parseStmtSeq :: ParserState -> ParserResult [AST.Statement]
parseStmtSeq ps 
  | current ps == L.EOF = 
      -- В случае обнаружения EOF в середине разбора последовательности операторов, 
      -- возвращаем пустой список операторов как результат и текущее состояние
      Right ([], ps)
  | current ps == L.EndKW  = pure ([], ps)
  | current ps == L.ElseKW = pure ([], ps)
  | current ps == L.Semicolon = do
      -- Пропускаем точку с запятой и продолжаем разбор
      ps1 <- Right (advance ps)
      tryParse parseStmtSeq ps1
  | otherwise = do
      -- Попытка разбора оператора
      case tryParse parseStmt ps of
        Left err -> 
          -- Если не получилось, восстанавливаемся и продолжаем
          let ps' = addError err ps
              ps'' = recover ps'
          in if currentPos ps == currentPos ps''
             then -- Если позиция не изменилась, значит не удалось восстановиться
                  -- В этом случае пропускаем текущий токен и продолжаем разбор
                  let newPs = advance ps
                  in Right ([], newPs)
             else tryParse parseStmtSeq ps''
        Right (s, ps1) -> do
          -- Точка с запятой после оператора необязательна
          ps2 <- case current ps1 of
            L.Semicolon -> Right (advance ps1)
            _ -> Right ps1
          (ss, ps3) <- tryParse parseStmtSeq ps2
          pure (s:ss, ps3)

parseStmt :: ParserState -> ParserResult AST.Statement
parseStmt ps = case current ps of
  L.IfKW    -> tryParse parseIf ps
  L.WhileKW -> tryParse parseWhile ps
  L.NewKW   -> tryParse parseNew ps
  L.Ident _ -> tryParse parseAssign ps
  _         -> Left $ AST.ParseError (currentPos ps) "ожидался оператор"

-- Assignment ::= Designator := Expression
parseAssign :: ParserState -> ParserResult AST.Statement
parseAssign ps = do
  (des, ps1) <- tryParse parseDesignator ps
  
  -- Обработка двух возможных вариантов синтаксиса присваивания:
  ps2 <- case current ps1 of
    L.Assign -> Right (advance ps1)
    L.Colon -> do
      ps' <- Right (advance ps1)
      case current ps' of
        L.Equal -> Right (advance ps')
        _ -> 
          let err = AST.ParseError (currentPos ps') "ожидался знак равенства после двоеточия для присваивания"
              ps'' = addError err ps'
          in Right (recover ps'')
    _ -> 
      let err = AST.ParseError (currentPos ps1) "ожидался оператор присваивания"
          ps' = addError err ps1
      in Right (recover ps')
      
  (e, ps3) <- tryParse parseExpr ps2
  pure (AST.Assignment des e, ps3)

-------------------------------------------------------------------------------
-- IF … THEN … [ELSE …] END ---------------------------------------------------
-------------------------------------------------------------------------------

parseIf :: ParserState -> ParserResult AST.Statement
parseIf ps = do
  ps1 <- Right (advance ps)  -- пропускаем IF
  
  (cond, ps2) <- tryParse parseExpr ps1
  
  ps3 <- case current ps2 of
    L.ThenKW -> Right (advance ps2)
    _ -> 
      let err = AST.ParseError (currentPos ps2) "ожидался THEN"
          ps' = addError err ps2
      in Right (recover ps')
      
  (thenB, ps4) <- tryParse parseStmtSeq ps3
  
  (elseB, ps5) <- case current ps4 of
    L.ElseKW -> do
      ps' <- Right (advance ps4)
      (es, ps'') <- tryParse parseStmtSeq ps'
      pure (Just es, ps'')
    _ -> pure (Nothing, ps4)
  
  ps6 <- case current ps5 of
    L.EndKW -> Right (advance ps5)
    _ -> 
      let err = AST.ParseError (currentPos ps5) "ожидался END"
          ps' = addError err ps5
      in Right (recover ps')
      
  pure (AST.IfStatement cond thenB elseB, ps6)

-------------------------------------------------------------------------------
-- WHILE … DO … END -----------------------------------------------------------
-------------------------------------------------------------------------------

parseWhile :: ParserState -> ParserResult AST.Statement
parseWhile ps = do
  ps1 <- Right (advance ps)  -- пропускаем WHILE
  
  (cond, ps2) <- tryParse parseExpr ps1
  
  ps3 <- case current ps2 of
    L.DoKW -> Right (advance ps2)
    _ -> 
      let err = AST.ParseError (currentPos ps2) "ожидался DO"
          ps' = addError err ps2
      in Right (recover ps')
      
  (body, ps4) <- tryParse parseStmtSeq ps3
  
  ps5 <- case current ps4 of
    L.EndKW -> Right (advance ps4)
    _ -> 
      let err = AST.ParseError (currentPos ps4) "ожидался END"
          ps' = addError err ps4
      in Right (recover ps')
      
  pure (AST.WhileStatement cond body, ps5)

-------------------------------------------------------------------------------
-- NEW ( designator ) ---------------------------------------------------------
-------------------------------------------------------------------------------

parseNew :: ParserState -> ParserResult AST.Statement
parseNew ps = do
  ps1 <- Right (advance ps)  -- пропускаем NEW
  
  ps2 <- case current ps1 of
    L.LParen -> Right (advance ps1)
    _ -> 
      let err = AST.ParseError (currentPos ps1) "ожидался ("
          ps' = addError err ps1
      in Right (recover ps')
      
  (des, ps3) <- tryParse parseDesignator ps2
  
  ps4 <- case current ps3 of
    L.RParen -> Right (advance ps3)
    _ -> 
      let err = AST.ParseError (currentPos ps3) "ожидался )"
          ps' = addError err ps3
      in Right (recover ps')
      
  pure (AST.NewStatement des, ps4)

-------------------------------------------------------------------------------
-- Expressions ----------------------------------------------------------------
-------------------------------------------------------------------------------

-- Токены для операций сравнения
relToks :: [L.Token]
relToks = [L.Equal, L.NotEqual, L.Less, L.LessEqual, L.Greater, L.GreaterEqual]

-- Преобразование токена в RelOp
tokToRel :: L.Token -> AST.RelOp
tokToRel L.Equal        = AST.Equal
tokToRel L.NotEqual     = AST.NotEqual
tokToRel L.Less         = AST.Less
tokToRel L.LessEqual    = AST.LessEqual
tokToRel L.Greater      = AST.Greater
tokToRel L.GreaterEqual = AST.GreaterEqual
tokToRel _              = error "Неверный токен отношения"

parseExpr :: ParserState -> ParserResult AST.Expression
parseExpr ps = do
  (lhs, ps1) <- tryParse parseSimpleExpr ps
  case current ps1 of
    tok | tok `elem` relToks -> do
      ps2 <- Right (advance ps1)
      (rhs, ps3) <- tryParse parseSimpleExpr ps2
      pure (AST.Relation lhs (tokToRel tok) rhs, ps3)
    _ -> pure (AST.SimpleExpr lhs, ps1)

-- Simple expression with optional sign and terms
parseSimpleExpr :: ParserState -> ParserResult AST.SimpleExpression
parseSimpleExpr ps = do
  -- Опциональный знак
  (sign, ps1) <- case current ps of
    L.Plus  -> Right (Just AST.Plus, advance ps)
    L.Minus -> Right (Just AST.Minus, advance ps)
    _       -> pure (Nothing, ps)
  
  -- Первый терм
  (term1, ps2) <- tryParse parseTerm ps1
  -- Остальные термы с операциями
  (terms, ps3) <- tryParse parseTermsWithOps ps2
  
  pure (AST.SimpleExpression sign (AST.TermWithOp term1 Nothing : terms), ps3)

-- Parse terms with operations (additive: +, -, OR)
parseTermsWithOps :: ParserState -> ParserResult [AST.TermWithOp]
parseTermsWithOps ps = case current ps of
  L.Plus -> do
    ps1 <- Right (advance ps)
    (term, ps2) <- tryParse parseTerm ps1
    (rest, ps3) <- tryParse parseTermsWithOps ps2
    pure (AST.TermWithOp term (Just AST.Add) : rest, ps3)
  L.Minus -> do
    ps1 <- Right (advance ps)
    (term, ps2) <- tryParse parseTerm ps1
    (rest, ps3) <- tryParse parseTermsWithOps ps2
    pure (AST.TermWithOp term (Just AST.Subtract) : rest, ps3)
  L.OrKW -> do
    ps1 <- Right (advance ps)
    (term, ps2) <- tryParse parseTerm ps1
    (rest, ps3) <- tryParse parseTermsWithOps ps2
    pure (AST.TermWithOp term (Just AST.Or) : rest, ps3)
  _ -> pure ([], ps)

-- Parse term (factors with multiplicative operations)
parseTerm :: ParserState -> ParserResult AST.Term
parseTerm ps = do
  (f, ps1) <- tryParse parseFactor ps
  (fs, ps2) <- tryParse parseFactorsWithOps ps1
  pure (AST.Term f fs, ps2)

-- Parse factors with operations (multiplicative: *, /, DIV, MOD, AND)
parseFactorsWithOps :: ParserState -> ParserResult [(AST.MulOp, Factor)]
parseFactorsWithOps ps = case current ps of
  L.Multiply -> do
    ps1 <- Right (advance ps)
    (f, ps2) <- tryParse parseFactor ps1
    (fs, ps3) <- tryParse parseFactorsWithOps ps2
    pure ((AST.Multiply, f) : fs, ps3)
  L.Divide -> do
    ps1 <- Right (advance ps)
    (f, ps2) <- tryParse parseFactor ps1
    (fs, ps3) <- tryParse parseFactorsWithOps ps2
    pure ((AST.Divide, f) : fs, ps3)
  L.DivKW -> do
    ps1 <- Right (advance ps)
    (f, ps2) <- tryParse parseFactor ps1
    (fs, ps3) <- tryParse parseFactorsWithOps ps2
    pure ((AST.Div, f) : fs, ps3)
  L.ModKW -> do
    ps1 <- Right (advance ps)
    (f, ps2) <- tryParse parseFactor ps1
    (fs, ps3) <- tryParse parseFactorsWithOps ps2
    pure ((AST.Mod, f) : fs, ps3)
  L.AndKW -> do
    ps1 <- Right (advance ps)
    (f, ps2) <- tryParse parseFactor ps1
    (fs, ps3) <- tryParse parseFactorsWithOps ps2
    pure ((AST.And, f) : fs, ps3)
  _ -> pure ([], ps)

-- Parse factor (atomic expressions)
parseFactor :: ParserState -> ParserResult AST.Factor
parseFactor ps = case current ps of
  L.IntLit n -> Right (AST.IntLiteral n, advance ps)
  L.RealLit n -> Right (AST.RealLiteral n, advance ps)
  L.LParen -> do
    ps1 <- Right (advance ps)
    (e, ps2) <- tryParse parseExpr ps1
    
    ps3 <- case current ps2 of
      L.RParen -> Right (advance ps2)
      _ -> 
        let err = AST.ParseError (currentPos ps2) "ожидался )"
            ps' = addError err ps2
        in Right (recover ps')
    
    pure (AST.ParenExpression e, ps3)
  L.NotKW -> do
    ps1 <- Right (advance ps)
    (f, ps2) <- tryParse parseFactor ps1
    pure (AST.NotFactor f, ps2)
  L.Ident _ -> do
    (d, ps1) <- tryParse parseDesignator ps
    pure (AST.DesignatorFactor d, ps1)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался фактор"

-- Parse designator (variable access path)
parseDesignator :: ParserState -> ParserResult AST.Designator
parseDesignator ps = case current ps of
  L.Ident name -> do
    ps1 <- Right (advance ps)
    (sels, ps2) <- tryParse parseSelectors ps1
    pure (AST.Designator name sels, ps2)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор"

-- Parse selectors (field access and dereference)
parseSelectors :: ParserState -> ParserResult [AST.Selector]
parseSelectors ps = case current ps of
  L.Period -> do
    ps1 <- Right (advance ps)
    case current ps1 of
      L.Ident field -> do
        ps2 <- Right (advance ps1)
        (rest, ps3) <- tryParse parseSelectors ps2
        pure (AST.FieldSelector field : rest, ps3)
      _ -> 
        let err = AST.ParseError (currentPos ps1) "ожидался идентификатор поля"
            ps' = addError err ps1
        in Right ([], recover ps')
  L.Caret -> do
    ps1 <- Right (advance ps)
    (rest, ps2) <- tryParse parseSelectors ps1
    pure (AST.Dereference : rest, ps2)
  _ -> pure ([], ps)




