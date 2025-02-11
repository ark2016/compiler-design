module AST (Program, Articles, Body, Instr(..)) where

-- | Тип Program: пара (articles, main body)
type Program = (Articles, Body)

-- | Articles представлены списком пар (имя, тело).
type Articles = [(String, Body)]

-- | Тело программы – список инструкций.
type Body = [Instr]

-- | Инструкция может быть словом, числом или условной конструкцией.
data Instr = IWord String
           | INum Int
           | IIf Body (Maybe Body)  -- if <then> [else <else>]
           deriving (Eq, Show)
