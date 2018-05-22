module Tokenize where

data Token = Lambda | Period | ParenLeft | ParenRight | Identifier String
  deriving Show

instance Eq Token where
  Lambda       == Lambda       = True
  Period       == Period       = True
  ParenLeft    == ParenLeft    = True
  ParenRight   == ParenRight   = True
  Identifier x == Identifier y = x == y
  _            == _            = False

lambdaToken :: Char
lambdaToken = '\\'

periodToken :: Char
periodToken = '.'

parenLeft :: Char
parenLeft = '('

parenRight :: Char
parenRight = ')'

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
                    | x == periodToken    = Period     : tokenizeExpr xs
                    | x == parenLeft      = ParenLeft  : tokenizeExpr xs
                    | x == parenRight     = ParenRight : tokenizeExpr xs
                    | isVariable x        = Identifier [x]    : tokenizeExpr xs
                    | x == longVarToken   = longIdentifier "" xs where
                      longIdentifier :: String -> String -> [Token]
                      longIdentifier _ []                                 = error "Unterminated long var!"
                      longIdentifier s (x:xs) | x == longVarToken         = Identifier s : tokenizeExpr xs
                                       | x `elem` longVarWhitelist = longIdentifier (s ++ [x]) xs
                                       | otherwise                 = error $ "Illegal long var character " ++ [x,'!']
tokenizeExpr (x:_)                        = error $ "Unrecognized token " ++ [x,'!']
