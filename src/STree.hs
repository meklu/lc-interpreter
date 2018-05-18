module STree where

data Expression =
  -- We provide some sort of integer identity for variables in addition to their label
  -- in order to more easily determine modulo alpha convertibility
  Variable { label :: Char, number :: Int } |
  -- These have a singular label, the label of the so-called binding variable
  -- `binding` should be a Variable!
  Abstraction { binding :: Expression, inner :: Expression } |
  -- The right side is given as a parameter to the left
  Application { left :: Expression, right :: Expression }
  deriving Show

foo :: String -> Int
foo x = 666
