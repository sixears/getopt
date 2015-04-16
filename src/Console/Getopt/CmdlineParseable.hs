{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Console.Getopt.CmdlineParseable
  ( CmdlineParseable(..), FileRO, getHandle )
where

-- base --------------------------------

import System.IO  ( Handle, IOMode( ReadMode ), openFile )
  
-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

--------------------------------------------------------------------------------
  
class CmdlineParseable a where
  enactOpt  :: String -> IO a

newtype FileRO = FRO { getHandle :: Handle }

instance Show FileRO where
  show fro = "FRO: " ++ show (getHandle fro)
           
instance NFData FileRO where

instance CmdlineParseable FileRO where
  enactOpt = (fmap FRO) . flip openFile ReadMode
  
instance (Read a) => CmdlineParseable a where
  enactOpt = return . read
