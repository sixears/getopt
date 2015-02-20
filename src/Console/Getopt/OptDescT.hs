{-# LANGUAGE TemplateHaskell #-}

{- |

Description : OptDesc type
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

OptDesc type
 
-}

module Console.Getopt.OptDescT
  -- OptDesc constructor and fields exported only for testing
  ( OptDesc(..), descn, dflt, lensname, munge, names, strt, summary, typename )
where

-- base --------------------------------

import Data.List    ( intercalate )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lenses ------------------------------

import Control.Lens  ( makeLenses )

-- template-haskell --------------------

import Language.Haskell.TH  ( ExpQ )

-- Fluffy --------------------------------------------------

import Fluffy.Language.TH            ( pprintQ )

-- this package --------------------------------------------

import Console.Getopt.OTypes  ( typeDefault )

--------------------------------------------------------------------------------

-- OptDesc =====================================================================

data OptDesc = OptDesc { _names     :: [String]       -- ^ cmdline option names
                       , _lensname  :: String         -- ^ name of accessor fn
                       , _typename  :: String         -- ^ client name of value
                                                      --   type
                       , _dflt      :: ExpQ           -- ^ option default value
                       , _strt      :: ExpQ           -- ^ option starting value
                       , _summary   :: String         -- ^ summary help doc
                       , _descn     :: String         -- ^ long help doc
                       , _munge     :: String -> ExpQ -- ^ val munger/checker
                       }

makeLenses ''OptDesc

-- Default -----------------------------

{- a proto-optVal; that is, the details used to build an optVal, but building
   them a bit at a time; so we can parse in bits of descriptor string and build
   up a OptDesc
-}

instance Default OptDesc where
  def  = OptDesc { _names    = []
                 , _lensname = ""
                 , _typename = "?Bool"
                 , _summary  = ""
                 , _descn    = ""
                 , _dflt     = [| Just False |]
                 , _strt     = [| Nothing |]
                 , _munge    = \_ -> [| return |]
                 }

-- Show --------------------------------

instance Show OptDesc where
  show OptDesc { _names    = names
               , _lensname = lensname
               , _typename = typename
               , _summary  = summary
               , _descn    = descn
               , _dflt     = dflt
               , _strt     = strt
               }
    = (concat . concat)
        [ [ intercalate "|" names ]
        , if lensname /= head names then [ ">", lensname ] else []
        , [ "::", typename, dfst ]
        , if null summary then [""] else ["#", summary]
        , if null descn then [""] else ["\n", descn]
        ]
      where df = pprintQ dflt
            st = pprintQ strt
            -- +-------------+----------+------------+
            -- | dflt | strt | uber-df  |    else    |
            -- +-------------+----------+------------+
            -- |   uber-df   |    ""    |  <><strt>  |
            -- +-------------+----------+------------+
            -- |     else    |  <dflt>  |<dflt><strt>|
            -- +-------------+----------+------------+
            -- is a given exp the uber-default for a type (if there is one)
            isTypeD :: ExpQ -> Bool
            isTypeD expq = case typeDefault typename of
                             Nothing -> False
                             -- comparing ExpQs just hangs, so compare the
                             -- prints
                             Just u  -> pprintQ expq == pprintQ u
            -- printed default & start value
            stStr = if st == "Data.Maybe.Nothing" then "" else "<" ++ st ++ ">"
            dfst = if isTypeD dflt
                   then if "" == stStr then "" else "<>" ++ stStr
                   else "<" ++ df ++ ">" ++ stStr