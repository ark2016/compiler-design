module AST where

data Program = Program {
    typeDeclarations :: [TypeDeclaration],
    varDeclarations :: [VarDeclaration],
    statements :: [Statement]
} deriving (Show) 

data TypeDeclaration = TypeDeclaration {
    typeName :: String,
    typeDefinition :: Type
} deriving (Show)

data Type = RealType
          | IntegerType
          | PointerType Type
          | RecordType {
              parentType :: Maybe String, 
              fields :: [FieldDeclaration]
            }
          | NamedType String 
          deriving (Show)

-- Объявление полей в записи
data FieldDeclaration = FieldDeclaration {
    fieldNames :: [String],
    fieldType :: Type
} deriving (Show)

-- Объявление переменных
data VarDeclaration = VarDeclaration {
    varNames :: [String],
    varType :: Type
} deriving (Show)

-- Операторы языка
data Statement = Assignment {
                   target :: Designator,
                   value :: Expression
                 }
               | IfStatement {
                   condition :: Expression,
                   thenBlock :: [Statement],
                   elseBlock :: Maybe [Statement] -- Опциональный блок else
                 }
               | WhileStatement {
                   whileCondition :: Expression,
                   whileBlock :: [Statement]
                 }
               | NewStatement {
                   newTarget :: Designator
                 }
               deriving (Show)
    
-- Выражения
data Expression = Relation {
                    leftExpr :: SimpleExpression,
                    relation :: RelOp,
                    rightExpr :: SimpleExpression
                  }
                | SimpleExpr SimpleExpression
                deriving (Show)

-- Операции сравнения
data RelOp = Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual
           deriving (Show)

-- Простое выражение
data SimpleExpression = SimpleExpression {
                         sign :: Maybe Sign, -- Опциональный знак
                         terms :: [TermWithOp]
                       }
                     deriving (Show)

data Sign = Plus | Minus
          deriving (Show)

data TermWithOp = TermWithOp {
                   term :: Term,
                   op :: Maybe AddOp -- Опциональная операция
                 }
               deriving (Show)

data AddOp = Add | Subtract | Or
           deriving (Show)

-- Терм (множители, связанные операторами умножения)
data Term = Term {
              factor :: Factor,
              restFactors :: [(MulOp, Factor)] -- Список пар: оператор и множитель
            }
          deriving (Show)

data MulOp = Multiply | Divide | Div | Mod | And
           deriving (Show)

-- Фактор (базовый элемент выражения)
data Factor = DesignatorFactor Designator
            | IntLiteral Integer
            | RealLiteral Double
            | ParenExpression Expression
            | NotFactor Factor
            deriving (Show)

-- Путь доступа (переменная или путь доступа к полю)
data Designator = Designator {
                    baseName :: String,
                    selectors :: [Selector]
                  }
                deriving (Show)

-- Селектор (шаг доступа)
data Selector = FieldSelector String  -- .field
              | Dereference           -- ^
              deriving (Show)

-- Тип токена с позицией в исходном коде
data Located a = Located {
                  pos :: (Int, Int),  -- строка и столбец
                  unLoc :: a
                }
              deriving (Show)

-- Тип ошибки для сообщений об ошибках
data ParseError = ParseError {
                    errorPos :: (Int, Int),
                    errorMsg :: String
                  }
                deriving (Show)

-- Класс для красивого форматированного вывода
class Pretty a where
  pretty :: Int -> a -> String  -- Int - уровень отступа

instance Pretty Program where
  pretty indent (Program types vars stmts) =
    "Program {\n" ++
    indent' ++ "TypeDeclarations:\n" ++ prettyList (indent + 2) types ++ "\n" ++
    indent' ++ "VarDeclarations:\n" ++ prettyList (indent + 2) vars ++ "\n" ++
    indent' ++ "Statements:\n" ++ prettyList (indent + 2) stmts ++ "\n" ++
    spaces indent ++ "}"
    where indent' = spaces (indent + 1)

instance Pretty TypeDeclaration where
  pretty indent (TypeDeclaration name def) =
    spaces indent ++ name ++ " = " ++ pretty (indent + 2) def

instance Pretty Type where
  pretty _ RealType = "REAL"
  pretty _ IntegerType = "INTEGER"
  pretty indent (PointerType t) = "POINTER TO " ++ pretty indent t
  pretty indent (RecordType parent fields) =
    "RECORD" ++ parentStr ++ " {\n" ++
    prettyList (indent + 2) fields ++ "\n" ++
    spaces indent ++ "}"
    where parentStr = case parent of
                        Nothing -> ""
                        Just p -> "(" ++ p ++ ")"
  pretty _ (NamedType name) = name

instance Pretty FieldDeclaration where
  pretty indent (FieldDeclaration names typ) =
    spaces indent ++ concat (zipWith (++) names (replicate (length names - 1) ", " ++ [""])) ++ 
    " : " ++ pretty indent typ

instance Pretty VarDeclaration where
  pretty indent (VarDeclaration names typ) =
    spaces indent ++ concat (zipWith (++) names (replicate (length names - 1) ", " ++ [""])) ++ 
    " : " ++ pretty indent typ

instance Pretty Statement where
  pretty indent (Assignment target expr) =
    spaces indent ++ pretty 0 target ++ " := " ++ pretty 0 expr
  pretty indent (IfStatement cond thenStmts elseStmts) =
    spaces indent ++ "IF " ++ pretty 0 cond ++ " THEN\n" ++
    prettyList (indent + 2) thenStmts ++
    case elseStmts of
      Nothing -> ""
      Just stmts -> "\n" ++ spaces indent ++ "ELSE\n" ++ prettyList (indent + 2) stmts
  pretty indent (WhileStatement cond body) =
    spaces indent ++ "WHILE " ++ pretty 0 cond ++ " DO\n" ++
    prettyList (indent + 2) body
  pretty indent (NewStatement target) =
    spaces indent ++ "NEW(" ++ pretty 0 target ++ ")"

instance Pretty Expression where
  pretty indent (Relation left op right) =
    pretty indent left ++ " " ++ pretty 0 op ++ " " ++ pretty 0 right
  pretty indent (SimpleExpr expr) = pretty indent expr

instance Pretty RelOp where
  pretty _ Equal = "="
  pretty _ NotEqual = "#"
  pretty _ Less = "<"
  pretty _ LessEqual = "<="
  pretty _ Greater = ">"
  pretty _ GreaterEqual = ">="

instance Pretty SimpleExpression where
  pretty indent (SimpleExpression sign terms) =
    signStr ++ concatMap (pretty indent) terms
    where signStr = case sign of
                      Nothing -> ""
                      Just Plus -> "+"
                      Just Minus -> "-"

instance Pretty TermWithOp where
  pretty indent (TermWithOp term op) =
    pretty indent term ++ opStr
    where opStr = case op of
                    Nothing -> ""
                    Just Add -> " + "
                    Just Subtract -> " - "
                    Just Or -> " OR "

instance Pretty Term where
  pretty indent (Term factor restFactors) =
    pretty indent factor ++ concatMap (\(op, f) -> " " ++ pretty 0 op ++ " " ++ pretty 0 f) restFactors

instance Pretty MulOp where
  pretty _ Multiply = "*"
  pretty _ Divide = "/"
  pretty _ Div = "DIV"
  pretty _ Mod = "MOD"
  pretty _ And = "&"

instance Pretty Factor where
  pretty indent (DesignatorFactor d) = pretty indent d
  pretty _ (IntLiteral n) = show n
  pretty _ (RealLiteral n) = show n
  pretty indent (ParenExpression e) = "(" ++ pretty indent e ++ ")"
  pretty indent (NotFactor f) = "~" ++ pretty indent f

instance Pretty Designator where
  pretty _ (Designator name selectors) =
    name ++ concatMap (\s -> pretty 0 s) selectors

instance Pretty Selector where
  pretty _ (FieldSelector field) = "." ++ field
  pretty _ Dereference = "^"

-- Вспомогательные функции
spaces :: Int -> String
spaces n = replicate n ' '

prettyList :: Pretty a => Int -> [a] -> String
prettyList indent = concatMap (\x -> spaces indent ++ pretty indent x ++ "\n")
-- Ограничение типа Pretty a => означает, что тип a должен иметь экземпляр класса Pretty

-- Функция для получения форматированного вывода AST
prettyPrint :: Program -> String
prettyPrint = pretty 0 

