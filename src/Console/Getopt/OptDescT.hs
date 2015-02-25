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

import Control.Applicative  ( (<*), (<*>), (*>), optional )
import Data.Char            ( isAlphaNum, isUpper )
import Data.Functor         ( (<$>) )
import Data.List            ( intercalate )
import Data.Maybe           ( fromMaybe )
import Text.Printf          ( printf )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lenses ------------------------------

import Control.Lens  ( (^.), makeLenses, set )

-- regex-applicative -------------------

import Text.Regex.Applicative  ( RE
                               , (<|>), anySym, many, psym, some, string, sym )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( Exp( AppE, LitE ), Lit( StringL ) )

-- fluffy --------------------------------------------------

import Fluffy.Language.TH  ( pprintQ )
import Fluffy.Language.TH.Type  ( readTS )
import Fluffy.Text.Regex   ( reFold )

-- this package --------------------------------------------

import Console.Getopt.OTypes    ( parser, startIsDefault, typeDefault
                                , typeStart, optionTypename )


--------------------------------------------------------------------------------

-- OptDesc =====================================================================

-- | a cmdline option described

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
  show  o -- OptDesc { _names    = names
               -- , _lensname = lensname
               -- , _typename = typename
               -- , _summary  = summary
               -- , _descn    = descn
               -- , _dflt     = dflt
               -- , _strt     = strt
               -- }
    = (concat . concat)
        [ [ intercalate "|"  (o ^. names) ]
        , if (o ^. lensname) /= head (o ^. names)
          then [ ">", o ^. lensname ] else []
        , [ "::", o ^. typename, dfst ]
        , if null (o ^. summary) then [""] else ["#", o ^. summary]
        , if null (o ^. descn) then [""] else ["\n", o ^. descn]
        ]
      where df = pprintQ (o ^. dflt)
            st = pprintQ (o ^. strt)
            -- +-------------+----------+------------+
            -- | dflt | strt | uber-df  |    else    |
            -- +-------------+----------+------------+
            -- |   uber-df   |    ""    |  <><strt>  |
            -- +-------------+----------+------------+
            -- |     else    |  <dflt>  |<dflt><strt>|
            -- +-------------+----------+------------+
            -- is a given exp the uber-default for a type (if there is one)
            isTypeD :: ExpQ -> Bool
            isTypeD expq = case typeDefault (o ^. typename) of
                             Nothing -> False
                             -- comparing ExpQs just hangs, so compare the
                             -- prints
                             Just u  -> pprintQ expq == pprintQ u
            -- printed default & start value
            stStr = if st == "Data.Maybe.Nothing" then "" else "<" ++ st ++ ">"
            dfst = if isTypeD (o ^. dflt)
                   then if "" == stStr then "" else "<>" ++ stStr
                   else "<" ++ df ++ ">" ++ stStr

-- Read --------------------------------

instance Read OptDesc where
  readsPrec = readsPrecOptDesc

-- setmunge ----------------------------

setmunge :: String -> OptDesc -> OptDesc
setmunge _ = undefined -- set munge $ _check m

-- errf --------------------------------

-- | errmsg for missing uber-default
errf :: String -- ^ default type - "start" or "default"
     -> String -- ^ name of type looked up
     -> String -- ^ full option text
     -> a
errf typ tt s =
  error $ printf "no uber-default for %s for type '%s' in opt '%s'" typ tt s

-- REGEXEN -----------------------------

-- | typename regex; /^::([][?.0-9A-Za-z]+)/

opttypename :: RE Char String
opttypename = string "::" *> some (psym (\c -> isAlphaNum c || c `elem` "[?]."))

-- | default value regex; /^([<({])[^(inverse \1)](inverse \1)/

optdflt :: RE Char String
optdflt =     sym '<' *> many (psym (/= '>')) <* sym '>'
          <|> sym '(' *> many (psym (/= ')')) <* sym ')'
          <|> sym '{' *> many (psym (/= '}')) <* sym '}'

-- | munge regex; /^!([^#]+)/

optmunge :: RE Char String
optmunge = sym '!' *> some (psym (/= '#'))

-- | type-default-startval-munge regex

optval :: RE Char (String, Maybe String, Maybe String, Maybe String)
optval = (,,,) <$> opttypename <*> optional optdflt
                               <*> optional optdflt
                               <*> optional optmunge

-- typeStartE --------------------------

-- | get the default starting value for a given user type string; error
--   if none available
typeStartE :: String -> String -> ExpQ
typeStartE t str = fromMaybe (errf "start" t str) (typeStart t)

-- startVal ----------------------------

-- | start value for implementation; parsed from user text if any, else from
--   type default
startVal :: Maybe String -- ^ start value, passed in by user between <>
         -> String       -- ^ user-requested option type
         -> String       -- ^ full user option type text
         -> ExpQ
startVal s t str = maybe (typeStartE t str) (readTS (optionTypename t)) s

-- typeDefaultE -----------------------

-- | like typeStartE, but for default
typeDefaultE :: String -> String -> ExpQ
typeDefaultE t str = fromMaybe (errf "default" t str) (typeDefault t)

-- defaultVal --------------------------

-- | like startVal, but for default
defaultVal :: Maybe String -> String -> String -> ExpQ
defaultVal s t str = maybe (typeDefaultE t str)
                           (return . AppE (parser t) .LitE . StringL) s

-- setTypeDfStMg -----------------------

-- | set the type, default, start value, munge of an OptDesc per values parsed
--   from an optstring
setTypeDfStMg :: String         -- ^ user string (for error messages)
              -> (String,
                  Maybe String,
                  Maybe String,
                  Maybe String) {- ^ user-requested type string;
                                     default value string, if any;
                                     start value string, if any;
                                     munge string, if any
                                 -}
              -> OptDesc -> OptDesc
        -- tt is base option type; df is Maybe default; st is maybe start;
        -- mg is Maybe munger
setTypeDfStMg str (type_str, mb_default_str, mb_start_str, mb_munge_str) =
  let df_val = (defaultVal mb_default_str type_str str)
  in    set typename type_str
      . -- take the start value; and pass it to set strt.  If there is
        -- neither a start nor a default; see if there's an uber-default
        -- for the type; if not, error
        (if startIsDefault type_str
         then -- start == default, so it's an error for start to be independently
              -- defined by he user
              maybe (set strt df_val)
                    (error $ printf "may not set start val with type %s (%s)"
                                    type_str str)
                   mb_start_str
         else
           set strt (startVal mb_start_str type_str str)
        )
      . set dflt df_val
      . maybe id setmunge mb_munge_str

-- pOptVal ---------------------------------------------------------------------

-- | parse optval.  Given a string, effect changes on an optdesc as specified by
--   that string.  Typical usage is to start with a default optdesc (that would
--   be specified by the empty string).

pOptVal :: String -> RE Char (OptDesc -> OptDesc)
pOptVal s = setTypeDfStMg s <$> optval

-- Read ------------------------------------------------------------------------

readsPrecOptDesc :: Int -> String -> [(OptDesc, String)]
readsPrecOptDesc _ s =
  let (o, r) = reFold def [ pIdNames
                          , pLensName
                          , pOptVal s
                          , pOptSumm
                          , pOptDescn ] s
      identifier :: RE Char [Char]
      identifier =  some $ psym (\c -> isAlphaNum c || c == '_')

      identifiers :: RE Char [String]
      identifiers =  (:) <$> identifier <*> many (sym '|' *> identifier)

      idnames :: [String] -> OptDesc -> OptDesc
      -- set lensname to first long option name by default
      idnames ss = set lensname (head ss) . set names ss

      pIdNames :: RE Char (OptDesc -> OptDesc)
      pIdNames = idnames <$> identifiers

      lensnm :: RE Char String
      lensnm =  sym '>' *> identifier

      pLensName :: RE Char (OptDesc -> OptDesc)
      pLensName =  set lensname <$> lensnm

      -- descn -----

      optdescn :: RE Char String
      -- deliberately allow empty commets if you don't want to document something
      -- (should mean the option is hidden, doesn't appear in --help)
      -- optdescn =  string "##" *> many anySym
      optdescn =  sym '\n' *> many anySym

      pOptDescn :: RE Char (OptDesc -> OptDesc)
      pOptDescn =  set descn <$> optdescn

      -- summary ---

      optsumm :: RE Char String
      -- deliberately allow empty comments if you don't want to document something
      -- XXX (should mean the option is hidden, doesn't appear in --help)
      optsumm =  sym '#' *> many (psym (/= '\n'))

      pOptSumm :: RE Char (OptDesc -> OptDesc)
      pOptSumm =  set summary <$> optsumm

   in if null $ o ^. names
      then error "no option name defined"
      -- returning [ (o,r) ] with a non-null r will cause
      -- a 'Prelude.read: no parse' error.  We can give a
      -- better error msg
      else if null r
           then if isUpper $ head (o ^. lensname)
                then error $ printf (    "lens '%s' may not begin with an "
                                      ++ "upper-case letter") (o ^. lensname)
                else [ (o,"") ]
           else error $ printf "failed to parse option '%s' at '%s'" s r

