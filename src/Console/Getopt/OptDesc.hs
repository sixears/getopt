{-# LANGUAGE TemplateHaskell #-}

{- |

Description : option descriptor access fns
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

option descriptor access fns

-}

module Console.Getopt.OptDesc
  ( OptDesc
  , names, name, lensname, summary, descn
  , dflt, dfGetter, dfltTxt, pclvField, pclvTypename
  , optSetVal, precordDefFields, recordFields
  , strt, typename
  , enactor

    -- exported for testing only
  , pprintQ
  )
where

-- base --------------------------------

import Control.Monad  ( liftM )
import Data.Maybe     ( fromMaybe )

-- lenses ------------------------------

import Control.Lens  ( (^.), view )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Exp( AppE, ConE, VarE ) )

-- Fluffy --------------------------------------------------

import Fluffy.Language.TH         ( composeE, nameE, pprintQ )

-- this package --------------------------------------------

import qualified Console.Getopt.OTypes as OTypes

import Console.Getopt.OptDescT      ( OptDesc, descn, dflt, lensname
                                    , names, strt, summary, typename )

--------------------------------------------------------------------------------

-- pclvTypename ------------------------

-- | the pclvTypename for this option

pclvTypename :: OptDesc -> String
pclvTypename = OTypes.pclvTypename . view typename

-- optionTypename ----------------------

optionTypename :: OptDesc -> String
optionTypename = OTypes.optionTypename . view typename

-- setter ------------------------------

setter :: OptDesc -> Exp
setter = OTypes.setter . view typename

-- enactor -----------------------------

-- | find the enactor for this option
enactor :: OptDesc -> Exp
enactor = OTypes.enactor . view typename

-- name --------------------------------

{- | a single name for this optdesc.  Within a valid set of optdescs, this
     should be unique (there is a check elsewhere against creating a set of
     options with overlapping names).  Will be a multi-character name if
     available
 -}
name :: OptDesc -> String
name o = let mcs = filter ((1<) . length) $ o ^. names -- multi-character names
          in case mcs of
               []   -> head $ o ^. names
               (m:_)-> m

-- dfltTxt -----------------------------

-- | String of the default value (for help text)
dfltTxt :: OptDesc -> String
dfltTxt = pprintQ . view dflt

-- pclvField ---------------------------

-- | name for the option field in the PCLV record

pclvField :: OptDesc -> String
pclvField = (++ "___") . view lensname

-- optSetVal ---------------------------

-- | the setval to use when setting this option in the PCLV record

optSetVal :: OptDesc -> ExpQ
optSetVal o =
  (o ^. strt) >>= \d -> return $ AppE (AppE (setter o) d) (nameE $ pclvField o)

-- viewE -------------------------------

-- | [q| view l |]

viewE :: String -> Exp
viewE  l = AppE (VarE 'view) (nameE l)

-- dfGetter

-- | a lambda that returns the user value, or default if such has been
--   defined and no user value was supplied
dfGetter :: OptDesc -> ExpQ
dfGetter o =
  let -- given a default value (df), return a fn that
      -- (1) takes on Opt (o)
      -- (2) reads a maybe value from the implementation field of the option
      -- (3) if it's a Just it unwraps it, else it returns the given default

      -- TH version of fromMaybe <default> . view <pclvField o>
      getter_mb  d = composeE (AppE (VarE 'fromMaybe) d) (viewE $ pclvField o)
   in liftM getter_mb (o ^. dflt) -- (o ^. dflt) >>= return . getter_mb

-- recordFields ----------------------------------------------------------------

{- | the fields passed to mkLensedRecordDef for this option; that is, the
     field name, field type and starting value for the implementation of the
     option record.  Fields are prefixed with '_' as they are later lensed.
 -}
recordFields :: OptDesc -> (String, String)
recordFields o = ('_' : o ^. lensname, optionTypename o)

-- precordDefFields ------------------------------------------------------------

{- | the fields passed to mkLensedRecordDef for the parsed record for this
     option; that is, the field name, field type and starting value for the
     parsed implementation of the option record.  Fields are prefixed with '_'
     as they are later lensed.
 -}
precordDefFields :: OptDesc -> (String, String, ExpQ)
precordDefFields o = ('_' : o ^. lensname ++ "___", 
                      pclvTypename o,
                      return $ ConE 'Nothing)
