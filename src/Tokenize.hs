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

identifierChars :: [Char]
identifierChars = ['a'..'z'] ++ ['A'..'Z']

isWhitespace :: Char -> Bool
isWhitespace x = x `elem` whitespaceChars

isIdentifier :: Char -> Bool
isIdentifier x = x `elem` identifierChars

-- ext.
longIdToken :: Char
longIdToken = '`'

longIdWhitelist :: [Char]
longIdWhitelist = identifierChars ++ " "

tokenizeExpr :: String -> [Token]
tokenizeExpr []                         = []
tokenizeExpr (x:xs) | isWhitespace x    = tokenizeExpr xs
                    | x == lambdaToken  = Lambda         : tokenizeExpr xs
                    | x == periodToken  = Period         : tokenizeExpr xs
                    | x == parenLeft    = ParenLeft      : tokenizeExpr xs
                    | x == parenRight   = ParenRight     : tokenizeExpr xs
                    | isIdentifier x    = Identifier [x] : tokenizeExpr xs
                    | x == longIdToken  = longIdentifier "" xs where
                        longIdentifier :: String -> String -> [Token]
                        longIdentifier _ []                                = error "Unterminated long identifier!"
                        longIdentifier s (x:xs) | x == longIdToken         = Identifier s : tokenizeExpr xs
                                                | x `elem` longIdWhitelist = longIdentifier (s ++ [x]) xs
                                                | otherwise                = error $ "Illegal long identifier character " ++ [x,'!']
tokenizeExpr (x:_)                      = error $ "Unrecognized token " ++ [x,'!']
