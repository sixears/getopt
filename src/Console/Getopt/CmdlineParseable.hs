{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- |

Description : command-line option parsing class
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

a class for cmdline parseable data types

 -}

module Console.Getopt.CmdlineParseable
  ( CmdlineParseable(..), FileRO, getHandle )
where

-- base --------------------------------

import System.IO  ( Handle, IOMode( ReadMode ), openFile )
  
-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

--------------------------------------------------------------------------------
  
{- | A type that may be parsed from the cmdline.  The purpose of this class is to
     provide an overridable default for how to parse, including any IO required.
     That default is @return . read@
 -}

class CmdlineParseable a where
  enactOpt  :: String -> IO a

-- FileRO ----------------------------------------------------------------------
  
-- | a CmdlineParseable file, in which a file path provided on the cmdline is
--   returned as a Handle which is opened RO (and thus fails at options-parsing
--   time if the open fails)

newtype FileRO = FRO { getHandle :: Handle -- ^ the encapsulated IO handle 
                     }

instance Show FileRO where
  show fro = "FRO: " ++ show (getHandle fro)
           
instance NFData FileRO where

instance CmdlineParseable FileRO where
  enactOpt = fmap FRO . flip openFile ReadMode
  
instance (Read a) => CmdlineParseable a where
  enactOpt = return . read
