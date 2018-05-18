module Tokenize where

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

-- ext.
longVarToken :: Char
longVarToken = '`'

longVarWhitelist :: [Char]
longVarWhitelist = variableChars ++ " "

tokenizeExpr :: String -> [Token]
tokenizeExpr []                           = []
tokenizeExpr (x:xs) | isWhitespace x      = tokenizeExpr xs
                    | x == lambdaToken    = Lambda     : tokenizeExpr xs
                    | x == abstrToken     = Abstr      : tokenizeExpr xs
                    | x == braceLeft      = BraceLeft  : tokenizeExpr xs
                    | x == braceRight     = BraceRight : tokenizeExpr xs
                    | isVariable x        = Var [x]    : tokenizeExpr xs
                    | x == longVarToken   = longVar "" xs where
                      longVar :: String -> String -> [Token]
                      longVar _ []                                 = error "Unterminated long var!"
                      longVar s (x:xs) | x == longVarToken         = Var s : tokenizeExpr xs
                                       | x `elem` longVarWhitelist = longVar (s ++ [x]) xs
                                       | otherwise                 = error $ "Illegal long var character " ++ [x,'!']
tokenizeExpr (x:_)                        = error $ "Unrecognized token " ++ [x,'!']
