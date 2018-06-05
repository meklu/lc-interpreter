module Lex where

import STree
import Tokenize

data LexState = LexState {
  parensLeft :: Int,
  parensRight :: Int,
  varCounter :: Int,
  greedy :: Bool,
  msg :: [String]
}
  deriving Show

-- gets an initial/"default" LexState
lexInit :: LexState
lexInit = LexState
  {
    parensLeft = 0,
    parensRight = 0,
    varCounter = 0,
    greedy = False,
    msg = []
  }

-- logs a message
lexLog :: LexState -> String -> LexState
lexLog state msg = state { msg = (msg : oldl) }
  where oldl = case state of (LexState { msg = l }) -> l

-- sets greediness, i.e. eat tokens until you die
lexSetGreedy :: LexState -> Bool -> LexState
lexSetGreedy state x = state { greedy = x }

-- gets greediness
lexGreedy :: LexState -> Bool
lexGreedy (LexState {greedy = g}) = g

-- gets varCounter
lexGetCounter :: LexState -> Int
lexGetCounter (LexState {varCounter = vc}) = vc

-- gets parensLeft
lexGetLeft :: LexState -> Int
lexGetLeft (LexState {parensLeft = pl}) = pl

-- gets parensRight
lexGetRight :: LexState -> Int
lexGetRight (LexState {parensRight = pr}) = pr

-- increments varCounter
lexIncrCounter :: LexState -> LexState
lexIncrCounter state = state { varCounter = (lexGetCounter state) + 1 }

-- increments parensLeft
lexIncrLeft :: LexState -> LexState
lexIncrLeft state = state { parensLeft = (lexGetLeft state) + 1 }

-- increments parensRight
lexIncrRight :: LexState -> LexState
lexIncrRight state = state { parensRight = (lexGetRight state) + 1 }

-- decrements parensLeft
lexDecrLeft :: LexState -> LexState
lexDecrLeft state = state { parensLeft = (lexGetLeft state) - 1 }

-- decrements parensRight
lexDecrRight :: LexState -> LexState
lexDecrRight state = state { parensRight = (lexGetRight state) - 1 }

-- checks whether the parens are balanced
lexBalanced :: LexState -> Bool
lexBalanced (LexState { parensLeft = pl, parensRight = pr}) = pl == pr

generateTree :: [Token] -> (LexState,Expression)
generateTree xs = subToAppl $ gen lexInit (normalizeTokens xs)

-- turns a SubExpression into an Application tree
subToAppl :: (LexState,Expression) -> (LexState,Expression)
subToAppl (s,SubExpression [])         = (s,Empty)
subToAppl (s,SubExpression (l:r:rest)) = let (ns,rx) = subToAppl (s,SubExpression rest) in (ns, applWrap (applWrap l r) rx)
subToAppl (s,SubExpression [x])        = let (ns,rx) = subToAppl (s,x)                  in (ns, rx)
subToAppl (s,Abstraction b x)          = let (ns,rx) = subToAppl (s,x)                  in (ns, Abstraction b rx)
-- probably not necessary but good to have just in case
subToAppl (s,Application l r)          = let (ns,ll) = subToAppl (s,l)
                                             (fs,rr) = subToAppl (ns,r)
                                         in  (fs,applWrap ll rr)
subToAppl r                            = r

normalizeTokens :: [Token] -> [Token]
-- turn \xy.<foo> into \x.\y.<foo>
normalizeTokens (Lambda:Identifier a:Identifier b:xs) = Lambda:Identifier a:Period:normalizeTokens (Lambda:Identifier b:xs)
normalizeTokens xs                                    = xs

applWrap :: Expression -> Expression -> Expression
applWrap Empty xpr                              = xpr
applWrap xpr Empty                              = xpr
applWrap xpr xpr2                               = Application xpr xpr2

genSub :: LexState -> [Token] -> (LexState,[Token],Expression)
genSub state []                                 = (state,[],Empty)
genSub state xs                                 = let (tokens,rest) = takeSub xs
                                                      (ns,xpr)      = gen state tokens
                                                  in  (lexLog ns ("genSub: " ++ show (tokens,rest)),rest,xpr)

takeSub :: [Token] -> ([Token],[Token])
takeSub xs = (takeWhile (/=ParenRight) xs, dropWhile (/=ParenRight) xs)

gen state ((ParenLeft):xs)                      = let (ns,rest,xpr) = genSub (lexIncrLeft state) xs
                                                      (ns2,xpr2)    = gen ns rest
                                                  in  (ns2,applWrap xpr xpr2)
gen state ((ParenRight):xs) | lexBalanced state = error "Closing paren makes the expression unbalanced!"
                            | otherwise         = gen (lexIncrRight state) xs
gen state (Lambda:Identifier bind:Period:xs)    = let
                                                    (ns,rest,inner) = genSub state xs
                                                    outer           = Abstraction (Variable bind (lexGetCounter ns)) inner
                                                    (fs,applxpr)    = gen (lexIncrCounter (lexSetGreedy ns False)) rest
                                                  in
                                                    (fs,applWrap outer applxpr)
gen state (Identifier x:xs)                     = let var        = Variable x (lexGetCounter state)
                                                      (ns,right) = gen (lexIncrCounter state) xs
                                                  in  (ns,applWrap var right)
-- FIXME: implement the rest
gen state (x:xs)                                = gen state xs
gen state _                                     = (state,Empty)

-- dbg
strLog :: LexState -> String
strLog (LexState {msg = l}) = concat . map (++"\n") $ l

-- this is rather hacky :D
showTree :: Expression -> String
showTree xpr = go 0 str
  where
    step = 3
    str = show xpr
    indKnot = ' ' : indKnot
    indT indent = take (indent * step) indKnot
    go indent ('{':xs)         = "{\n" ++ (indT (indent + 1)) ++ go (indent + 1) xs
    go indent ('}':',':' ':xs) = "\n" ++ indT (indent - 1) ++ "},\n" ++ indT (indent - 1) ++ go (indent - 1) xs
    go indent ('}':'}':xs)     = "\n" ++ indT (indent - 1) ++ "}" ++ go (indent - 1) ('}':xs)
    go indent ('}':xs)         = "\n" ++ indT (indent - 1) ++ "}\n" ++ indT (indent - 1) ++ go (indent - 1) xs
    go indent ( x :xs)         = x : go indent xs
    go indent _                = []

dbgTree :: String -> String
dbgTree expr = let (state,xpr) = generateTree (tokenizeExpr expr)
               in  strLog state ++ "\nxpr = " ++ showTree xpr ++ "\n"
