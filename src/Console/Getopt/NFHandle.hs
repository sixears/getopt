{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Description : handles that are subject to NFData
Copyright   : (c) Martyn J. Pearce 2015
License     : BSD
Maintainer  : haskell@sixears.com

Handle that is instance of NFData

 -}

module Console.Getopt.NFHandle
  ( NFHandle(NFHandle), unhandle )
where

-- base --------------------------------
  
import System.IO  ( Handle )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- NFHandle --------------------------------------------------------------------

-- | Handle that is an instance of NFData.  We use a newtype here to avoid an
--   orphaned type.

newtype NFHandle = NFHandle Handle
  deriving Eq
instance Show NFHandle where
  show = show . unhandle
instance NFData NFHandle where

-- | extract handle
unhandle :: NFHandle -> Handle
unhandle (NFHandle h) = h


