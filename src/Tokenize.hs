module Tokenize where

import STree

data Token = Lambda | Abstr | BraceLeft | BraceRight

lambdaToken :: Char
lambdaToken = '\\'

abstrToken :: Char
abstrToken = '.'

braceLeft :: Char
braceLeft = '('

braceRight :: Char
braceRight = ')'

whitespace :: [Char]
whitespace = "\t\n\r "

generateTree :: String -> Expression
generateTree = undefined

