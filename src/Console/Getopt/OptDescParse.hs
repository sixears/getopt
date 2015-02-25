{-# LANGUAGE TemplateHaskell #-}

{- |

Description : parsing a string into an OptDesc structure
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

option descriptor access fns

-}

module Console.Getopt.OptDescParse
  ( _check )
where

-- base --------------------------------

import Control.Applicative  ( (<*), (<*>), (*>), optional )
import Data.Char            ( isAlphaNum, isUpper )
import Data.Functor         ( (<$>) )
import Data.Maybe           ( fromMaybe )
import Text.Printf          ( printf )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lenses ------------------------------

import Control.Lens  ( (^.), set )

-- regex-applicative -------------------

import Text.Regex.Applicative  ( RE
                               , (<|>), anySym, many, psym, some, string, sym )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( newName
                                   , Exp( AppE, LamE, LitE, VarE )
                                   , Lit( IntegerL, StringL )
                                   , Pat( VarP )
                                   )

-- fluffy ------------------------------

import Fluffy.Data.List         ( splitBy2 )
import Fluffy.Language.TH       ( catchQ, checkBoolString, nameE )
import Fluffy.Language.TH.Type  ( readT, readTS )
import Fluffy.Text.Regex        ( reFold )

-- this package --------------------------------------------

import Console.Getopt.OptDescT  ( OptDesc
                                , descn, dflt, lensname, munge
                                , names, strt, summary, typename )
import Console.Getopt.OTypes    ( parser, startIsDefault, typeDefault
                                , typeStart, optionTypename )

--------------------------------------------------------------------------------

-- setmunge ----------------------------

setmunge :: String -> OptDesc -> OptDesc
setmunge m = set munge $ _check m

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

instance Read OptDesc where
  readsPrec = readsPrecOptDesc

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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

readInt :: String -> Integer
readInt  = $(readT "Integer")

-- _check ----------------------------------------------------------------------

{- | verify an option value.

     First arg string which is used to create the check; if it contains a space,
     it will be treated as a leading operator and a trailing integer value;
     e.g., ">= 0" will check that the value is non-negative.  If it does not
     contain a space, it is treated as a function x -> Bool; the option value
     is passed in, and a False return will be considered as a verification
     failure.

     Second arg string is the name of the option as invoked; to use in the error
     message if the verification fails.
-}

-- > runQ [| \a -> if op a val then return a else fail $ printf "%s" (show a) |]
-- ===>
-- LamE [VarP a_6]
--      (CondE (AppE (AppE (VarE op_0) (VarE a_6)) (VarE val_0))
--             (AppE (VarE GHC.Base.return) (VarE a_6))
--             (InfixE (Just (VarE GHC.Base.fail))
--                     (VarE GHC.Base.$)
--                     (Just (AppE (AppE (VarE Text.Printf.printf)
--                                       (LitE (StringL "%s")))
--                                 (AppE (VarE GHC.Show.show) (VarE a_6))
--                           ))))

_check :: String -> String -> ExpQ
_check checkstr s =
  let a' = newName "a"
      (op_t, val_t) = splitBy2 (== ' ') checkstr
      (val, errmsg) =
        if null val_t
        then (0, printf "option %s val %%s failed check: '%s'" s op_t)
        else (readInt val_t,
              printf "option %s val %%s failed check: %s %s"
                     s op_t (show val)
             )
      errmsg_e      = LitE $ StringL errmsg
      -- f :: Exp( Bool ); e :: Exp( String )
      -- [| \a -> if f then return a else fail $ printf errmsg_e (show a) |]
--X--            validate a f e = (CondE f
--X--                                    (AppE (VarE 'return) (VarE a))
--X--                                    (InfixE (Just (VarE 'fail))
--X--                                            (VarE '($))
--X--                                            (Just (AppE (AppE (VarE 'printf) e)
--X--                                                        (AppE (VarE 'show)
--X--                                                              (VarE a))
--X--                                                  ))))

   in -- [| \a -> if op a val -- if a > 0 -- op a val
      --          then return a
      --          else fail $ printf "option %s val %s failed check: %s %s"
      --                             s (show a) op_t (show val) |]
      do a <- a'
         let f = -- Exp ( :: Bool )
                 if null val_t
                 then AppE (nameE op_t) (VarE a)
                 else AppE (AppE (nameE op_t) (VarE a))
                           (LitE (IntegerL (readInt val_t)))
--               c <- catchQ f -- Exp ( :: IO (Bool, String) )
         c <- catchQ (AppE (VarE 'return) f) -- Exp ( :: IO (Bool, String) )
--               c <- ((appE (varE 'return) f') >>= catchQ) -- Exp ( :: IO (Bool, String) )
--               m <- checkBoolString c errmsg_e -- Exp( Maybe String )
         m <- checkBoolString (return c) (return $ AppE (AppE (VarE 'printf) errmsg_e) (AppE (VarE 'show) (VarE a))) -- Exp( Maybe String )
         -- > runQ [| do (b,s) <- f;
         --           return $ if null s then "yes" else "no" |]
         -- ===>
         -- DoE [ BindS (TupP [ VarP b, VarP s ]) (VarE 'f)
         --     , NoBindS (InfixE (Just (VarE 'return))
         --                       (VarE '($))
         --                       (Just (CondE (AppE (VarE GHC.List.null)
         --                                          (VarE s))
         --                                    (LitE (StringL "yes"))
         --                                    (LitE (StringL "no")))))
         --     ]
--               return $ LamE [VarP a] (validate a f errmsg_e)
         return $ LamE [VarP a] m
         -- > runQ [| do (b,s) <- f;
         --           return $ if null s then "yes" else "no" |]
         -- ===>
         -- DoE [ BindS (TupP [ VarP b, VarP s ]) (VarE 'f)
         --     , NoBindS (InfixE (Just (VarE 'return))
         --                       (VarE '($))
         --                       (Just (CondE (AppE (VarE GHC.List.null)
         --                                          (VarE s))
         --                                    (LitE (StringL "yes"))
         --                                    (LitE (StringL "no")))))
         --     ]

