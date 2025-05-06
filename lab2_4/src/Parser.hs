{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

-- | Recursive‑descent parser for the Oberon subset (lab 2.4).
--   All lexer tokens are imported *qualified* (alias @L@) to avoid
--   clashes with identically‑named constructors in the AST.
module Parser where

import           AST                       hiding (RelOp (..), Sign (..))
import qualified AST                       as AST
import           Lexer                     (Token (..))
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

parseProgram :: [AST.Located L.Token] -> Either AST.ParseError Program
parseProgram toks = do
  let ps0 = initParser toks
  (prog, ps1) <- parseProgram' ps0
  if isEOF ps1 then pure prog
               else Left $ AST.ParseError (currentPos ps1) "лишние символы после END."

-------------------------------------------------------------------------------
-- PROGRAM := ( TYPE … )? ( VAR … )? BEGIN stmtSeq END . ---------------------
-------------------------------------------------------------------------------

parseProgram' :: ParserState -> ParserResult Program
parseProgram' ps = do
  (types, ps1) <- case current ps of
    L.TypeKW -> match L.TypeKW ps >>= parseTypeDecls
    _        -> pure ([], ps)
  (vars , ps2) <- case current ps1 of
    L.VarKW  -> match L.VarKW ps1 >>= parseVarDecls
    _        -> pure ([], ps1)
  ps3            <- match L.BeginKW ps2
  (body, ps4)    <- parseStmtSeq ps3
  ps5            <- match L.EndKW ps4 >>= match L.Period >>= match L.EOF
  pure (Program types vars body, ps5)

-------------------------------------------------------------------------------
-- TYPE declarations ----------------------------------------------------------
-------------------------------------------------------------------------------

parseTypeDecls :: ParserState -> ParserResult [TypeDeclaration]
parseTypeDecls ps
  | current ps `elem` [L.VarKW, L.BeginKW] = pure ([], ps)
  | otherwise = do
      (d , ps1) <- parseTypeDecl ps
      ps2       <- match L.Semicolon ps1
      (ds, ps3) <- parseTypeDecls ps2
      pure (d:ds, ps3)

parseTypeDecl :: ParserState -> ParserResult TypeDeclaration
parseTypeDecl ps = case current ps of
  L.Ident name -> do
    ps1        <- match (L.Ident name) ps
    ps2        <- match L.Equal ps1
    (ty, ps3)  <- parseType ps2
    pure (TypeDeclaration name ty, ps3)
  _ -> Left $ AST.ParseError (currentPos ps) "ожидался идентификатор типа"

-- Type ::= REAL | INTEGER | POINTER TO Type | RECORD … END
parseType :: ParserState -> ParserResult Type
parseType ps = case current ps of
  L.RealKW    -> match L.RealKW ps    >>= \ps' -> pure (RealType,    ps')
  L.IntegerKW -> match L.IntegerKW ps >>= \ps' -> pure (IntegerType, ps')
  L.PointerKW -> do
    ps1         <- match L.PointerKW ps >>= match L.ToKW
    (base, ps2) <- parseType ps1
    pure (PointerType base, ps2)
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
    pure (RecordType parent fields, ps4)
  _ -> Left $ AST.ParseError (currentPos ps) "неизвестный тип"

parseFieldDecls :: ParserState -> ParserResult [FieldDeclaration]
parseFieldDecls ps
  | current ps == L.EndKW = pure ([], ps)
  | otherwise             = do
      (fd , ps1) <- parseFieldDecl ps
      ps2        <- match L.Semicolon ps1
      (fds, ps3) <- parseFieldDecls ps2
      pure (fd:fds, ps3)

parseFieldDecl :: ParserState -> ParserResult FieldDeclaration
parseFieldDecl ps = do
  (ids, ps1) <- parseIdentList ps
  ps2        <- match L.Colon ps1
  (ty, ps3)  <- parseType ps2
  pure (FieldDeclaration ids ty, ps3)

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

parseVarDecls :: ParserState -> ParserResult [VarDeclaration]
parseVarDecls ps
  | current ps == L.BeginKW = pure ([], ps)
  | otherwise               = do
      (vd , ps1) <- parseVarDecl ps
      ps2        <- match L.Semicolon ps1
      (vds, ps3) <- parseVarDecls ps2
      pure (vd:vds, ps3)

parseVarDecl :: ParserState -> ParserResult VarDeclaration
parseVarDecl ps = do
  (ids, ps1) <- parseIdentList ps
  ps2        <- match L.Colon ps1
  (ty , ps3) <- parseType ps2
  pure (VarDeclaration ids ty, ps3)

-------------------------------------------------------------------------------
-- Statements -----------------------------------------------------------------
-------------------------------------------------------------------------------

parseStmtSeq :: ParserState -> ParserResult [Statement]
parseStmtSeq ps = case current ps of
  L.EndKW  -> pure ([], ps)
  L.ElseKW -> pure ([], ps)
  _        -> do
    (s , ps1) <- parseStmt ps
    (ss, ps2) <- parseStmtSeq ps1
    pure (s:ss, ps2)

parseStmt :: ParserState -> ParserResult Statement
parseStmt ps = case current ps of
  L.IfKW    -> parseIf ps
  L.WhileKW -> parseWhile ps
  L.NewKW   -> parseNew ps
  L.Ident _ -> parseAssign ps
  _         -> Left $ AST.ParseError (currentPos ps) "ожидался оператор"

-- Assignment ::= Designator := Expression
parseAssign :: ParserState -> ParserResult Statement
parseAssign ps = do
  (des, ps1) <- parseDesignator ps
  ps2        <- match L.Assign ps1
  (e  , ps3) <- parseExpr ps2
  pure (Assignment des e, ps3)

-------------------------------------------------------------------------------
-- IF … THEN … [ELSE …] END ---------------------------------------------------
-------------------------------------------------------------------------------

parseIf :: ParserState -> ParserResult Statement
parseIf ps = do
  ps1            <- match L.IfKW ps
  (cond , ps2)   <- parseExpr ps1
  ps3            <- match L.ThenKW ps2
  (thenB, ps4)   <- parseStmtSeq ps3
  (elseB, ps5)   <- case current ps4 of
    L.ElseKW -> match L.ElseKW ps4 >>= parseStmtSeq >>= \(es, ps') -> pure (Just es, ps')
    _        -> pure (Nothing, ps4)
  ps6            <- match L.EndKW ps5
  pure (IfStatement cond thenB elseB, ps6)

-------------------------------------------------------------------------------
-- WHILE … DO … END -----------------------------------------------------------
-------------------------------------------------------------------------------

parseWhile :: ParserState -> ParserResult Statement
parseWhile ps = do
  ps1           <- match L.WhileKW ps
  (cond, ps2)   <- parseExpr ps1
  ps3           <- match L.DoKW ps2
  (body, ps4)   <- parseStmtSeq ps3
  ps5           <- match L.EndKW ps4
  pure (WhileStatement cond body, ps5)

-------------------------------------------------------------------------------
-- NEW ( designator ) ---------------------------------------------------------
-------------------------------------------------------------------------------

parseNew :: ParserState -> ParserResult Statement
parseNew ps = do
  ps1         <- match L.NewKW ps
  ps2         <- match L.LParen ps1
  (des, ps3)  <- parseDesignator ps2
  ps4         <- match L.RParen ps3
  pure (NewStatement des, ps4)

-------------------------------------------------------------------------------
-- Expressions ----------------------------------------------------------------
-------------------------------------------------------------------------------

parseExpr :: ParserState -> ParserResult Expression
parseExpr ps = do
  (lhs, ps1) <- parseSimpleExpr ps
  case current ps1 of
    tok | tok `elem` relToks -> do
      ps2           <- match tok ps1
      (rhs, ps3)    <- parseSimpleExpr ps2
      pure (Relation lhs (tokToRel tok) rhs, ps3)
    _ -> pure (SimpleExpr lhs

