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
  }

type ParserResult a = Either AST.ParseError (a, ParserState)

initParser :: [AST.Located L.Token] -> ParserState
initParser []       = error "Parser: empty token list"
initParser (t : ts) = ParserState ts t

current :: ParserState -> L.Token
current = AST.unLoc . currentToken

currentPos :: ParserState -> (Int,Int)
currentPos = AST.pos . currentToken

advance :: ParserState -> ParserState
advance ParserState{tokens = [], currentToken = t}   = ParserState [] t
advance ParserState{tokens = t:ts}                   = ParserState ts t

match :: L.Token -> ParserState -> Either AST.ParseError ParserState
match expected ps@ParserState{currentToken = AST.Located p actual}
  | expected == actual = Right (advance ps)
  | otherwise = Left $ AST.ParseError p ("ожидался " ++ show expected ++ ", получен " ++ show actual)

isEOF :: ParserState -> Bool
isEOF = (== L.EOF) . current

-------------------------------------------------------------------------------
-- Entry point ----------------------------------------------------------------
-------------------------------------------------------------------------------

parseProgram :: [AST.Located L.Token] -> Either AST.ParseError AST.Program
parseProgram toks = do
  let ps0 = initParser toks
  (prog, ps1) <- parseProgram' ps0
  -- После успешного разбора программы, следующий токен должен быть EOF
  case current ps1 of
    L.EOF -> pure prog
    _     -> Left $ AST.ParseError (currentPos ps1) "лишние символы после END."

-------------------------------------------------------------------------------
-- PROGRAM := ( TYPE … )? ( VAR … )? BEGIN stmtSeq END . ---------------------
-------------------------------------------------------------------------------

parseProgram' :: ParserState -> ParserResult AST.Program
parseProgram' ps = do
  (types, ps1) <- case current ps of
    L.TypeKW -> match L.TypeKW ps >>= parseTypeDecls
    _        -> pure ([], ps)
  (vars , ps2) <- case current ps1 of
    L.VarKW  -> match L.VarKW ps1 >>= parseVarDecls
    _        -> pure ([], ps1)
  ps3            <- match L.BeginKW ps2
  (body, ps4)    <- parseStmtSeq ps3
  ps5            <- match L.EndKW ps4 >>= match L.Period
  pure (AST.Program types vars body, ps5)

-------------------------------------------------------------------------------
-- TYPE declarations ----------------------------------------------------------
-------------------------------------------------------------------------------

parseTypeDecls :: ParserState -> ParserResult [AST.TypeDeclaration]
parseTypeDecls ps
  | current ps `elem` [L.VarKW, L.BeginKW] = pure ([], ps)
  | otherwise = do
      (d , ps1) <- parseTypeDecl ps
      ps2       <- match L.Semicolon ps1
      (ds, ps3) <- parseTypeDecls ps2
      pure (d:ds, ps3)

parseTypeDecl :: ParserState -> ParserResult AST.TypeDeclaration
parseTypeDecl ps = case current ps of
  L.Ident name -> do
    ps1        <- match (L.Ident name) ps
    ps2        <- match L.Equal ps1
    (ty, ps3)  <- parseType ps2
    pure (AST.TypeDeclaration name ty, ps3)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор типа"

-- Type ::= REAL | INTEGER | POINTER TO Type | RECORD … END
parseType :: ParserState -> ParserResult AST.Type
parseType ps = case current ps of
  L.RealKW    -> match L.RealKW ps    >>= \ps' -> pure (AST.RealType,    ps')
  L.IntegerKW -> match L.IntegerKW ps >>= \ps' -> pure (AST.IntegerType, ps')
  L.PointerKW -> do
    ps1         <- match L.PointerKW ps >>= match L.ToKW
    (base, ps2) <- parseType ps1
    pure (AST.PointerType base, ps2)
  L.RecordKW  -> do
    ps1 <- match L.RecordKW ps
    (parent, ps2) <- case current ps1 of
      L.LParen -> do
        ps' <- match L.LParen ps1
        case current ps' of
          L.Ident base -> match (L.Ident base) ps' >>= match L.RParen >>= \ps'' -> pure (Just base, ps'')
          _            -> Left $ AST.ParseError (currentPos ps') "ожидался идентификатор базового типа"
      _ -> pure (Nothing, ps1)
    (fields, ps3) <- parseFieldDecls ps2
    ps4           <- match L.EndKW ps3
    pure (AST.RecordType parent fields, ps4)
  L.Ident name -> do
    -- Обработка для именованных типов (пользовательских типов)
    ps1 <- match (L.Ident name) ps
    pure (AST.NamedType name, ps1)
  _ -> Left $ AST.ParseError (currentPos ps) "неизвестный тип"

parseFieldDecls :: ParserState -> ParserResult [AST.FieldDeclaration]
parseFieldDecls ps
  | current ps == L.EndKW = pure ([], ps)
  | otherwise             = do
      (fd , ps1) <- parseFieldDecl ps
      ps2        <- match L.Semicolon ps1
      (fds, ps3) <- parseFieldDecls ps2
      pure (fd:fds, ps3)

parseFieldDecl :: ParserState -> ParserResult AST.FieldDeclaration
parseFieldDecl ps = do
  (ids, ps1) <- parseIdentList ps
  ps2        <- match L.Colon ps1
  (ty, ps3)  <- parseType ps2
  pure (AST.FieldDeclaration ids ty, ps3)

parseIdentList :: ParserState -> ParserResult [String]
parseIdentList ps = case current ps of
  L.Ident nm -> do
    ps1 <- match (L.Ident nm) ps
    case current ps1 of
      L.Comma -> do
        ps2            <- match L.Comma ps1
        (rest, ps3)    <- parseIdentList ps2
        pure (nm:rest, ps3)
      _       -> pure ([nm], ps1)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор"

-------------------------------------------------------------------------------
-- VAR declarations -----------------------------------------------------------
-------------------------------------------------------------------------------

parseVarDecls :: ParserState -> ParserResult [AST.VarDeclaration]
parseVarDecls ps
  | current ps == L.BeginKW = pure ([], ps)
  | otherwise               = do
      (vd , ps1) <- parseVarDecl ps
      ps2        <- match L.Semicolon ps1
      (vds, ps3) <- parseVarDecls ps2
      pure (vd:vds, ps3)

parseVarDecl :: ParserState -> ParserResult AST.VarDeclaration
parseVarDecl ps = do
  (ids, ps1) <- parseIdentList ps
  ps2        <- match L.Colon ps1
  (ty , ps3) <- parseType ps2
  pure (AST.VarDeclaration ids ty, ps3)

-------------------------------------------------------------------------------
-- Statements -----------------------------------------------------------------
-------------------------------------------------------------------------------

parseStmtSeq :: ParserState -> ParserResult [AST.Statement]
parseStmtSeq ps = case current ps of
  L.EndKW  -> pure ([], ps)
  L.ElseKW -> pure ([], ps)
  L.Semicolon -> do
    -- Пропускаем точку с запятой и продолжаем разбор
    ps1 <- match L.Semicolon ps
    parseStmtSeq ps1
  _        -> do
    -- Попытка разбора оператора
    case parseStmt ps of
      Left _ -> 
        -- Если не получилось, возвращаем пустую последовательность
        pure ([], ps)
      Right (s, ps1) -> do
        -- Точка с запятой после оператора необязательна
        ps2 <- case current ps1 of
          L.Semicolon -> match L.Semicolon ps1
          _ -> pure ps1
        (ss, ps3) <- parseStmtSeq ps2
        pure (s:ss, ps3)

parseStmt :: ParserState -> ParserResult AST.Statement
parseStmt ps = case current ps of
  L.IfKW    -> parseIf ps
  L.WhileKW -> parseWhile ps
  L.NewKW   -> parseNew ps
  L.Ident _ -> parseAssign ps
  _         -> Left $ AST.ParseError (currentPos ps) "ожидался оператор"

-- Assignment ::= Designator := Expression
parseAssign :: ParserState -> ParserResult AST.Statement
parseAssign ps = do
  (des, ps1) <- parseDesignator ps
  -- Обработка двух возможных вариантов синтаксиса присваивания:
  -- 1. Токен Assign (если лексер корректно распознал его)
  -- 2. Последовательность токенов Colon и Equal (если лексер обработал их как отдельные)
  ps2 <- case current ps1 of
    L.Assign -> match L.Assign ps1
    L.Colon -> do
      ps' <- match L.Colon ps1
      case current ps' of
        L.Equal -> match L.Equal ps'
        _ -> Left $ AST.ParseError (currentPos ps') "ожидался знак равенства после двоеточия для присваивания"
    _ -> Left $ AST.ParseError (currentPos ps1) "ожидался оператор присваивания"
  (e, ps3) <- parseExpr ps2
  pure (AST.Assignment des e, ps3)

-------------------------------------------------------------------------------
-- IF … THEN … [ELSE …] END ---------------------------------------------------
-------------------------------------------------------------------------------

parseIf :: ParserState -> ParserResult AST.Statement
parseIf ps = do
  ps1            <- match L.IfKW ps
  (cond , ps2)   <- parseExpr ps1
  ps3            <- match L.ThenKW ps2
  (thenB, ps4)   <- parseStmtSeq ps3
  (elseB, ps5)   <- case current ps4 of
    L.ElseKW -> match L.ElseKW ps4 >>= parseStmtSeq >>= \(es, ps') -> pure (Just es, ps')
    _        -> pure (Nothing, ps4)
  ps6            <- match L.EndKW ps5
  pure (AST.IfStatement cond thenB elseB, ps6)

-------------------------------------------------------------------------------
-- WHILE … DO … END -----------------------------------------------------------
-------------------------------------------------------------------------------

parseWhile :: ParserState -> ParserResult AST.Statement
parseWhile ps = do
  ps1           <- match L.WhileKW ps
  (cond, ps2)   <- parseExpr ps1
  ps3           <- match L.DoKW ps2
  (body, ps4)   <- parseStmtSeq ps3
  ps5           <- match L.EndKW ps4
  pure (AST.WhileStatement cond body, ps5)

-------------------------------------------------------------------------------
-- NEW ( designator ) ---------------------------------------------------------
-------------------------------------------------------------------------------

parseNew :: ParserState -> ParserResult AST.Statement
parseNew ps = do
  ps1         <- match L.NewKW ps
  ps2         <- match L.LParen ps1
  (des, ps3)  <- parseDesignator ps2
  ps4         <- match L.RParen ps3
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
  (lhs, ps1) <- parseSimpleExpr ps
  case current ps1 of
    tok | tok `elem` relToks -> do
      ps2           <- match tok ps1
      (rhs, ps3)    <- parseSimpleExpr ps2
      pure (AST.Relation lhs (tokToRel tok) rhs, ps3)
    _ -> pure (AST.SimpleExpr lhs, ps1)

-- Simple expression with optional sign and terms
parseSimpleExpr :: ParserState -> ParserResult AST.SimpleExpression
parseSimpleExpr ps = do
  -- Опциональный знак
  (sign, ps1) <- case current ps of
    L.Plus  -> match L.Plus ps  >>= \ps' -> pure (Just AST.Plus, ps')
    L.Minus -> match L.Minus ps >>= \ps' -> pure (Just AST.Minus, ps')
    _       -> pure (Nothing, ps)
  
  -- Первый терм
  (term1, ps2) <- parseTerm ps1
  -- Остальные термы с операциями
  (terms, ps3) <- parseTermsWithOps ps2
  
  pure (AST.SimpleExpression sign (AST.TermWithOp term1 Nothing : terms), ps3)

-- Parse terms with operations (additive: +, -, OR)
parseTermsWithOps :: ParserState -> ParserResult [AST.TermWithOp]
parseTermsWithOps ps = case current ps of
  L.Plus -> do
    ps1        <- match L.Plus ps
    (term, ps2) <- parseTerm ps1
    (rest, ps3) <- parseTermsWithOps ps2
    pure (AST.TermWithOp term (Just AST.Add) : rest, ps3)
  L.Minus -> do
    ps1        <- match L.Minus ps
    (term, ps2) <- parseTerm ps1
    (rest, ps3) <- parseTermsWithOps ps2
    pure (AST.TermWithOp term (Just AST.Subtract) : rest, ps3)
  L.OrKW -> do
    ps1        <- match L.OrKW ps
    (term, ps2) <- parseTerm ps1
    (rest, ps3) <- parseTermsWithOps ps2
    pure (AST.TermWithOp term (Just AST.Or) : rest, ps3)
  _ -> pure ([], ps)

-- Parse term (factors with multiplicative operations)
parseTerm :: ParserState -> ParserResult AST.Term
parseTerm ps = do
  (f, ps1) <- parseFactor ps
  (fs, ps2) <- parseFactorsWithOps ps1
  pure (AST.Term f fs, ps2)

-- Parse factors with operations (multiplicative: *, /, DIV, MOD, AND)
parseFactorsWithOps :: ParserState -> ParserResult [(AST.MulOp, Factor)]
parseFactorsWithOps ps = case current ps of
  L.Multiply -> do
    ps1        <- match L.Multiply ps
    (f, ps2)   <- parseFactor ps1
    (fs, ps3)  <- parseFactorsWithOps ps2
    pure ((AST.Multiply, f) : fs, ps3)
  L.Divide -> do
    ps1        <- match L.Divide ps
    (f, ps2)   <- parseFactor ps1
    (fs, ps3)  <- parseFactorsWithOps ps2
    pure ((AST.Divide, f) : fs, ps3)
  L.DivKW -> do
    ps1        <- match L.DivKW ps
    (f, ps2)   <- parseFactor ps1
    (fs, ps3)  <- parseFactorsWithOps ps2
    pure ((AST.Div, f) : fs, ps3)
  L.ModKW -> do
    ps1        <- match L.ModKW ps
    (f, ps2)   <- parseFactor ps1
    (fs, ps3)  <- parseFactorsWithOps ps2
    pure ((AST.Mod, f) : fs, ps3)
  L.AndKW -> do
    ps1        <- match L.AndKW ps
    (f, ps2)   <- parseFactor ps1
    (fs, ps3)  <- parseFactorsWithOps ps2
    pure ((AST.And, f) : fs, ps3)
  _ -> pure ([], ps)

-- Parse factor (atomic expressions)
parseFactor :: ParserState -> ParserResult AST.Factor
parseFactor ps = case current ps of
  L.IntLit n -> match (L.IntLit n) ps >>= \ps' -> pure (AST.IntLiteral n, ps')
  L.RealLit n -> match (L.RealLit n) ps >>= \ps' -> pure (AST.RealLiteral n, ps')
  L.LParen -> do
    ps1       <- match L.LParen ps
    (e, ps2)  <- parseExpr ps1
    ps3       <- match L.RParen ps2
    pure (AST.ParenExpression e, ps3)
  L.NotKW -> do
    ps1       <- match L.NotKW ps
    (f, ps2)  <- parseFactor ps1
    pure (AST.NotFactor f, ps2)
  L.Ident _ -> do
    (d, ps1)  <- parseDesignator ps
    pure (AST.DesignatorFactor d, ps1)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался фактор"

-- Parse designator (variable access path)
parseDesignator :: ParserState -> ParserResult AST.Designator
parseDesignator ps = case current ps of
  L.Ident name -> do
    ps1           <- match (L.Ident name) ps
    (sels, ps2)   <- parseSelectors ps1
    pure (AST.Designator name sels, ps2)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор"

-- Parse selectors (field access and dereference)
parseSelectors :: ParserState -> ParserResult [AST.Selector]
parseSelectors ps = case current ps of
  L.Period -> do
    ps1 <- match L.Period ps
    case current ps1 of
      L.Ident field -> do
        ps2         <- match (L.Ident field) ps1
        (rest, ps3) <- parseSelectors ps2
        pure (AST.FieldSelector field : rest, ps3)
      _ -> Left $ AST.ParseError (currentPos ps1) "ожидался идентификатор поля"
  L.Caret -> do
    ps1         <- match L.Caret ps
    (rest, ps2) <- parseSelectors ps1
    pure (AST.Dereference : rest, ps2)
  _ -> pure ([], ps)




