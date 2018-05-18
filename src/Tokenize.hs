module Tokenize where

import STree

data Token = Lambda | Abstr | BraceLeft | BraceRight | Var String
  deriving Show

lambdaToken :: Char
lambdaToken = '\\'

abstrToken :: Char
abstrToken = '.'

braceLeft :: Char
braceLeft = '('

braceRight :: Char
braceRight = ')'

whitespaceChars :: [Char]
whitespaceChars = "\t\n\r "

variableChars :: [Char]
variableChars = ['a'..'z'] ++ ['A'..'Z']

isWhitespace :: Char -> Bool
isWhitespace x = x `elem` whitespaceChars

isVariable :: Char -> Bool
isVariable x = x `elem` variableChars

tokenizeExpr :: String -> [Token]
tokenizeExpr []                           = []
tokenizeExpr (x:xs) | isWhitespace x      = tokenizeExpr xs
                    | x == lambdaToken    = Lambda     : tokenizeExpr xs
                    | x == abstrToken     = Abstr      : tokenizeExpr xs
                    | x == braceLeft      = BraceLeft  : tokenizeExpr xs
                    | x == braceRight     = BraceRight : tokenizeExpr xs
                    | isVariable x        = Var [x]    : tokenizeExpr xs
tokenizeExpr (x:_)                        = error $ "Unrecognized token " ++ [x] ++ "!"

generateTree :: [Token] -> Expression
generateTree = undefined
