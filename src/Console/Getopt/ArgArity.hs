{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK prune,show-extensions #-}

{-|

Description : describe arg count accepted by Getopt
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

describe arg count accepted by Getopt
 
 -}

module Console.Getopt.ArgArity
  ( ArgArity(..), check_arity, liftAA, show_arity )
where
  
-- base --------------------------------
  
import Data.List    ( intercalate )
import Text.Printf  ( printf )

-- template-haskell --------------------

import Language.Haskell.TH         ( Exp( AppE, ConE ) )
import Language.Haskell.TH.Syntax  ( Lift( lift ) )

-- Fluffy ------------------------------

import Fluffy.Language.TH  ( intE, mAppE )

--------------------------------------------------------------------------------
-- ArgArity
--------------------------------------------------------------------------------

-- | how many args does this program take?

data ArgArity = ArgNone
              | ArgOne
              | ArgN Int
              | ArgMaybe        -- ^ zero or one
              | ArgAny          -- ^ zero or more
              | ArgMany         -- ^ one or more
              | ArgSome Int Int -- ^ between m & n inclusive
              | ArgMin Int      -- ^ at least n args
              | ArgMax Int      -- ^ no more than n args


liftAA :: ArgArity -> Exp
liftAA ArgNone       = ConE 'ArgNone
liftAA ArgOne        = ConE 'ArgOne
liftAA (ArgN n)      = AppE (ConE 'ArgN) (intE n)
liftAA ArgMaybe      = ConE 'ArgMaybe
liftAA ArgAny        = ConE 'ArgAny
liftAA ArgMany       = ConE 'ArgMany
liftAA (ArgSome m n) = mAppE [ConE 'ArgSome, intE m, intE n]
liftAA (ArgMin m)    = AppE (ConE 'ArgMin) (intE m)
liftAA (ArgMax m)    = AppE (ConE 'ArgMax) (intE m)

instance Lift ArgArity where
  lift = return . liftAA

-- show_arity ------------------------------------------------------------------

-- | given an arg type and arity, return a string to express the arg/arity to 
--   users

show_arity :: ArgArity -> String -> String

show_arg :: String -> String
show_arg s = "<" ++ s ++ ">"
show_arity ArgNone       _  =  ""
show_arity _             "" =  ""
-- ArgOne is deliberately just the string, so this can be used e.g., as a 
-- default for helpme (users can supply their own "<foo!+>"
show_arity ArgOne        s  =  show_arg s
show_arity (ArgN 1)      s  =  show_arity ArgOne s
show_arity (ArgN n)      s  =  show_arg s ++ "{" ++ show n ++ "}"
show_arity (ArgSome m n) s  =  printf "%s{%d,%d}" (show_arg s) m n
show_arity ArgMaybe      s  =  show_arg s ++ "?"
show_arity ArgAny        s  =  show_arg s ++ "*"
show_arity ArgMany       s  =  show_arg s ++ "+"
show_arity (ArgMin n)    s  =  show_arg s ++ "{" ++ show n ++ "+}"
show_arity (ArgMax n)    s  =  show_arg s ++ "{<=" ++ show n ++ "}"

-- check_arity -----------------------------------------------------------------

-- | return an error string if #args does not match arity

check_arity :: ArgArity -> [String] -> Maybe String

_got :: [String] -> String
_got xs = let xs' = intercalate "," $ fmap (printf "'%s'") xs
           in if 0 == length xs
              then " (got none)"
              else printf " (got %d: %s)" (length xs) xs'

_arge :: String -> [String] -> Maybe String
_arge s xs = Just $ s ++ _got xs

check_arity ArgNone [] = Nothing
check_arity ArgNone xs  = _arge "brooks no argument" xs

check_arity ArgOne [_] = Nothing
check_arity ArgOne []  = Just "wants an argument"
check_arity ArgOne xs  = _arge "wants a single argument" xs

check_arity (ArgN 0) xs = check_arity ArgNone xs
check_arity (ArgN 1) xs = check_arity ArgOne xs
check_arity (ArgN n) xs = if n == length xs
                          then Nothing
                          else _arge (printf "takes %d arguments" n) xs

check_arity (ArgSome m n) xs =
  if m <= length xs && n >= length xs
  then Nothing
  else _arge (printf "takes between %d & %d arguments (inclusive)" m n) xs

check_arity ArgMaybe []  = Nothing
check_arity ArgMaybe [_] = Nothing
check_arity ArgMaybe xs  = _arge "takes 0 or 1 arguments" xs

check_arity ArgAny _ = Nothing

check_arity ArgMany [] = Just "wants one or more arguments"
check_arity ArgMany _  = Nothing

check_arity (ArgMin n) xs = 
  if n > length xs
  then _arge (printf "requires at least %d arguments" n) xs
  else Nothing
       
check_arity (ArgMax n) xs = 
  if n < length xs
  then _arge (printf "requires at no more than %d arguments" n) xs
  else Nothing

