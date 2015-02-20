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
  parseAs :: Monad m => String -> String -> m p
  
instance ParseOpt String where
  parseAs _ = return

instance (Show p, Read p) => ParseOpt p where
  parseAs t = return . readType t

