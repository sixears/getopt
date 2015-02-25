{-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances #-}

{- |

Description : typeclass for parsing options from the cmd line
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

typeclass for parsing options from the cmd line

-}

-- a typeclass for parsing options from the command line.  Isolated in this file
-- because of the use of FlexibleInstances, OverlappingInstances,
-- UndecidableInstances; to limit their influence to where it is needed

module Console.Getopt.ParseOpt
  ( parseAs )
where

import Fluffy.Language.TH.Type  ( readType )

class ParseOpt p where
  -- | how to parse a cmdline string;
  parseAs :: Monad m => String -- ^ "type" to parse as, as a string (may be a
                               --   non-Haskell type; rather a getopt pseudo-type
                               --   a la @incr@ or @filero@
                     -> String -- ^ the cmdline string to parse
                     -> m p

-- | handle strings specially - we don't expect strings to be quoted when read
--   from the cmdline
instance ParseOpt String where
  parseAs _ = return

-- | use our readType to provide slightly more informative errors than 'read'
instance (Show p, Read p) => ParseOpt p where
  parseAs t = return . readType t

