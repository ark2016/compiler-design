module Lexer (Token(..), tokenize) where

import Text.Read (readMaybe)

-- | Токен может быть числом или словом.
data Token = TokNum Int | TokWord String
  deriving (Eq, Show)

-- | Функция tokenize разбивает строку по пробельным символам.
-- Если токен можно преобразовать в Int, он становится числом.
tokenize :: String -> [Token]
tokenize input = map toToken (words input)
  where
    toToken s = case readMaybe s :: Maybe Int of
                  Just n  -> TokNum n
                  Nothing -> TokWord s
