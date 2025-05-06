module AST where

-- Программа состоит из опционального раздела объявлений типов,
-- опционального раздела объявлений переменных и обязательного блока операторов
data Program = Program {
    typeDeclarations :: [TypeDeclaration],
    varDeclarations :: [VarDeclaration],
    statements :: [Statement]
} deriving (Show)

-- Объявление типа связывает имя с определением типа
data TypeDeclaration = TypeDeclaration {
    typeName :: String,
    typeDefinition :: Type
} deriving (Show)

-- Различные виды типов
data Type = RealType
          | IntegerType
          | PointerType Type
          | RecordType {
              parentType :: Maybe String,
              fields :: [FieldDeclaration]
            }
          | NamedType String -- Именованный пользовательский тип
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
                   elseBlock :: Maybe [Statement]
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
                         sign :: Maybe Sign,
                         terms :: [TermWithOp]
                       }
                     deriving (Show)

data Sign = Plus | Minus
          deriving (Show)

data TermWithOp = TermWithOp {
                   term :: Term,
                   op :: Maybe AddOp
                 }
               deriving (Show)

data AddOp = Add | Subtract | Or
           deriving (Show)

-- Терм (множители, связанные операторами умножения)
data Term = Term {
              factor :: Factor,
              restFactors :: [(MulOp, Factor)]
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
