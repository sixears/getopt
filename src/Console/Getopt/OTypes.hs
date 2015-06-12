{-# LANGUAGE MultiParamTypeClasses
           , RankNTypes
           , TemplateHaskell
  #-}

-- in comments, [q| xxx |] is intended to mean 'like [| xxx |], but outside
-- of the Q monad

{- |

Description : embedded 'option types' for use in OptDesc descriptor strings
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

embedded 'option types' for use in OptDesc descriptor strings

-}

module Console.Getopt.OTypes
  ( pclvType, pclvTypename, parser, setter, setter_st, enactor, startIsDefault
  , typeDefault, typeStart, optionTypename, optionType )
where

-- to make a thing use maybe:
-- -) add "Maybe " ++ to pclvTypename
-- -) list the field in dfGetter'
-- -) adjust the setter to use the maybeized setter
-- -) compose Just onto the front of the parser
-- -) compose Just onto the front of the defaults

-- base --------------------------------

import Data.Char   ( isUpper )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- template-haskell --------------------

import Language.Haskell.TH  ( Exp ( AppE, ConE, LitE, SigE, VarE               )
                            , Lit ( IntegerL, StringL                          )
                            , Type( AppT, ArrowT, ConT                         )
                            , ExpQ
                            , mkName
                            )

-- fluffy ------------------------------

import Fluffy.Language.TH       ( composeE, mAppE, stringE )
import Fluffy.Language.TH.Type  ( readType, strToT )

-- this package --------------------------------------------

import Console.Getopt                   ( setval
                                        , setvalcM, setvalc'M
                                        , setvalsM
                                        )
import Console.Getopt.CmdlineParseable  ( FileRO, enactOpt )
import Console.Getopt.ParseOpt          ( parseAs )

--------------------------------------------------------------------------------


-- | return type for oTypes function; info about how we handle option-target
--   pseudo-types (e.g., incr, or ?Int)

-- so we take a command-line string, parse it with parser_, set it
-- with setter_ , into the PCLV record.  And we use
-- enactor to take the PCLV field or the uber-default, and set that into the
-- OV record.

data OptTypes = OptTypes { {- | the type used for the PCLV container field.
                                Note that this will be prefixed with "Maybe" by
                                the `pclvTypename` fn, since we need to handle
                                option defaults
                            -}
                           pclvTypename_    :: String -- e.g., "Maybe FilePath"
                           -- | the type used for the OV container field
                         , optionTypename_  :: String -- e.g., "Handle"
                           {- | how to take a value that has been parsed by
                                parser_, and store it into the PCLV record.
                                Thus, no IO.  This one will need setval or
                                similar to set the value in the record.
                            -}
                         , setter_          :: Exp -- e.g., [q| setval return |]
                           {- | how to take a value that has been parsed by
                                parser_, and store it into the PCLV record.
                                Thus, no IO.  This one will need setval or
                                similar to set the value in the record.
                            -}
                         , setter_st_       :: Maybe Exp
                           {- | how to parse a String to generate a value; as an
                                Exp (:: String -> optionType).  This also
                                includes parsing a default value given in the
                                string to mkopts.  But the _st_ is because we
                                pass in a start value to initialize the lens
                                when it is called (but not beforehand, so we
                                may determine an uncalled opt)
                            -}
                         , parser_          :: Exp -- e.g., readParser "<type>"
                           {- | how to take a value, perform IO on it to provide
                                a user value.  This must be capable of handling
                                default values, too (so that, say, a default
                                filero of "/etc/motd" doesn't do its IO unless
                                the default is actually used)
                            -}
                         , enactor_         :: Exp -- e.g., [q| openFileRO |]
                           {- | The value to use as the default for an option,
                                if no default is provided to mkopts (in <...>).
                                If Nothing, then it is an error to fail to
                                provide a default to mkopts.
                            -}
                         , default_         :: Maybe Exp -- e.g., Just [q| 0 |]
                           {- | The value to use as the start value for an
                                option, if none is provided to mkopts (in a
                                second(?) pair of <...>).
                                If Nothing, then there is no start value for
                                this type; it starts with Nothing or empty list
                                or empty map.
                            -}
                         , start_           :: Maybe Exp -- e.g., Just [q| 0 |]
                           {- | If true, there are no distinct start & default;
                                the start value is the default value.  E.g., for
                                incr, it makes no sense to have different start
                                & default values.
                            -}
                         , startIsDefault_  :: Bool
                         }
  deriving Show

-- def ---------------------------------

instance Default OptTypes where
  def = OptTypes { pclvTypename_   = "-UNIMPLEMENTED IMPLEMENTATION TYPE-"
                 , optionTypename_ = "-UNIMPLEMENTED OPTION TYPE-"
                 , setter_         = stringE "-UNIMPLEMENTED SETTER-"
                 , setter_st_      = Nothing
                 , parser_         = stringE "-UNIMPLEMENTED PARSER-"
                 , enactor_        = stringE "-UNIMPLEMENTED ENACTOR-"
                 , default_        = Just $ ConE 'Nothing
                 , start_          = Just $ ConE 'Nothing
                 , startIsDefault_ = False
                 }


--------------------------------------------------------------------------------

-- | \ t -> [q| readType t :: String -> t |]
readParser :: String -> Exp
readParser t = SigE (AppE (VarE 'readType) (LitE (StringL t)))
                    (AppT (AppT ArrowT (ConT ''String)) (strToT t))

readInt :: Exp
readInt = readParser "Int"

-- | open a file in read-only mode

openFileRO :: FilePath -> IO FileRO
openFileRO = enactOpt

-- | call setval on a thing parsed to type t
setval_as :: String -> Exp
setval_as t = -- [q| setval (parseAs t) |]
              AppE (VarE 'setval) (AppE (VarE 'parseAs) (stringE t))

------------------------------------------------------------

oTypes :: String -> OptTypes
oTypes s = let o = oTypes_ s in o -- traceShow o o

oTypes_ :: String -> OptTypes

oTypes_ "incr" = def { pclvTypename_   = "Int"
                     , optionTypename_ = "Int"
                      -- we need a parser to parse a potential default value
                     , setter_st_      = Just $ VarE 'setvalcM
                     , parser_         = readInt
                     , enactor_        = VarE 'return
                     , default_        = Just (LitE (IntegerL 0))
                     , start_          = Just (AppE (ConE 'Just)
                                                    (LitE (IntegerL 0)))
                     , startIsDefault_ = True
                     }

oTypes_ "decr" = def { pclvTypename_   = "Int"
                     , optionTypename_ = "Int"
                     , setter_st_      = Just $ VarE 'setvalc'M
                      -- we need a parser to parse a potential default value
                     , parser_         = readInt
                     , enactor_        = VarE 'return
                     , default_        = Just (AppE (ConE 'Just)
                                                    (LitE (IntegerL 0)))
                     , start_          = Just (AppE (ConE 'Just) (LitE (IntegerL 0)))
                     , startIsDefault_ = True
                     }

oTypes_ "filero" = def { pclvTypename_   = "FilePath"
                       , optionTypename_ = "FileRO"
                       , setter_         = AppE (VarE 'setval) (VarE 'return)
                       , parser_         = VarE 'id
                       , enactor_        = VarE 'openFileRO
                       }

oTypes_ ('?' : '*' : t@(h :_))
                | isUpper h =
                  def { pclvTypename_   = "String"
                      , optionTypename_ = '?' : t
                      , setter_         = setval_as t
                      , parser_         = VarE 'id
                        -- [q| maybe (return Nothing) ((fmap Just) . enactOpt) |]
                      , enactor_        =
                          mAppE [ VarE 'maybe
                                , AppE (VarE 'return) (ConE 'Nothing)
                                , composeE (AppE (VarE 'fmap) (ConE 'Just))
                                                              (VarE 'enactOpt)
                                ]
                      }

                | otherwise = error $ "no such option type: '?*" ++ t ++ "'"

oTypes_ ('?':t) = def { pclvTypename_   = t
                      , optionTypename_ = '?' : t
                      , setter_         = setval_as t
                      , parser_         = readParser t
                      , enactor_        = VarE 'return
                      }

--Y oTypes_ tt@('[' : '*' : t@(h :_))
--Y   | isUpper h && last t == ']' =
--Y       def { pclvTypename_   = '[' : t
--Y           , optionTypename_ = '[' : t
--Y           , setter_         = VarE 'setvals
--Y           , enactor_        = VarE 'enactOpt
--Y           , parser_         = VarE 'id
--Y           , default_        = Just $ ConE '[]
--Y           , start_          = Just $ ConE '[]
--Y           }
--Y
--Y   | otherwise = error $ "no such option type: '" ++ tt ++ "'"

--X oTypes_ ('[':',':t) | last t == ']' = oTypes_ ("[<,>" ++ t)
--X oTypes_ tt@('[':'<':s) | '>' `elem` s && last s == ']' =
--X         let (d,t') = splitOn2 ">" s
--X             t      = init t'
--X             list_t = "[" ++ t ++ "]"
--X          in def { pclvTypename_   = list_t
--X                 , optionTypename_ = list_t
--X                 , setter_         = -- [q| setvals' d (parseAs t) |]
--X                                     AppE (AppE (VarE 'setvals') (stringE d))
--X                                          (AppE (VarE 'parseAs) (stringE t))
--X                 , parser_         = readParser list_t
--X                 , enactor_        = VarE 'return
--X                 , default_        = Just $ ConE '[]
--X                 , start_          = Just $ ConE '[]
--X                 }
--X                        | otherwise = error $ "no such option type: '" ++ tt
--X                                                                       ++ "'"

oTypes_ tt@('[':t)
  | not (null t) && (isUpper (head t) || '[' == head t) && last t == ']' =
        def { pclvTypename_   = tt
            , optionTypename_ = tt
            , setter_st_      = -- [q| setvalsM (parseAs t) |]
                                Just $ AppE (VarE 'setvalsM)
                                            (AppE (VarE 'parseAs) (stringE tt))
            , parser_         = readParser tt
            , enactor_        = VarE 'return
            , default_        = Just $ (AppE (ConE 'Just) (ConE '[])) -- Just (ConE 'Nothing)
            , start_          = Just (ConE '[]) -- Just (ConE 'Nothing) -- Just $ (AppE (ConE 'Just) (ConE '[]))
            }

--X oTypes_ tt@('[':t)
--X   | not (null t) && isUpper (head t) && last t == ']' =
--X         def { pclvTypename_   = tt
--X             , optionTypename_ = tt
--X             , setter_         = -- [q| setvals (parseAs t) |]
--X                                 AppE (VarE 'setvals)
--X                                      (AppE (VarE 'parseAs) (stringE t))
--X             , parser_         = (readParser tt)
--X             , enactor_        = VarE 'return
--X             , default_        = Just $ ConE '[]
--X             , start_          = Just $ ConE '[]
--X             }

-- IO types (simple,  value as String)
oTypes_ ('*' : t@(h : _))
  | isUpper h = def { pclvTypename_   = "String"
                    , optionTypename_ = t
                    , setter_         = setval_as t
                    , parser_         = VarE 'id
                    , enactor_        = VarE 'enactOpt
                    , default_        = Nothing
                    }

  | otherwise = error $ "no such option type: '*" ++ t ++ "'"

oTypes_ [] = error "empty typestring"

oTypes_ t@(h:_) | isUpper h =
                 def { pclvTypename_   = t
                     , optionTypename_ = t
                     , setter_         = setval_as t
                     , parser_         = readParser t
                     , enactor_        = VarE 'return
                     , default_        =
                         case t of
                           "String" -> Just . LitE $ StringL ""
                           "Int"    -> Just . LitE $ IntegerL 0
                           _        -> Nothing
                     -- since values are set rather than updated, it makes
                     -- no sense to have a start /= default
                     , startIsDefault_ = True
                     }

                | otherwise = error $ "no such option type: '" ++ t ++ "'"

-- typeDefault -----------------------------------------------------------------

-- | uber-default value for named type
typeDefault :: String -> Maybe ExpQ
typeDefault = fmap return . default_ . oTypes


-- typeStart -------------------------------------------------------------------

-- | uber-default start value for named type
typeStart :: String -> Maybe ExpQ
typeStart = fmap return . start_ . oTypes

-- pclvTypename -------------------------------------------------------------------

-- | type to use within the PCLV record for a requested option type
--   (so Int becomes Maybe Int (to allow for default != start) for example)
pclvTypename :: String -> String
pclvTypename = ("Maybe " ++) . pclvTypename_ . oTypes

-- pclvType --------------------------------------------------------------------

-- | type to use within the PCLV record for a requested option type
--   (so Int becomes Maybe Int (to allow for default != start) for example)
pclvType :: String -> Type
pclvType = ConT . mkName . pclvTypename

-- optionTypename -----------------------------------------------------------------

-- | type to represent to the getopt developer for a requested option type (so
--   incr becomes Int, for example)
optionTypename :: String -> String
optionTypename = optionTypename_ . oTypes

-- optionType ------------------------------------------------------------------

-- | type to represent to the getopt developer for a requested option type (so
--   incr becomes Int, for example)
optionType :: String -> Type
optionType = ConT . mkName . optionTypename

-- setter ----------------------------------------------------------------------

{- | how to take a value that has been parsed by parser_, and store it into the
     PCLV record. Thus, no IO.  This one will need setval or similar to set the
     value in the record.
 -}

setter :: String -> Exp
setter = setter_ . oTypes

-- setter ----------------------------------------------------------------------

{- | how to take a value that has been parsed by parser_, and store it into the
     PCLV record. Thus, no IO.  This one will need setval or similar to set the
     value in the record.  It expects a start value as its first argument.
 -}

setter_st :: String -> Maybe Exp
setter_st = setter_st_ . oTypes

-- parser ----------------------------------------------------------------------

{- | how to parse a String to generate a value; as an
     Exp (:: String -> optionType).  This also includes parsing a default value
     given in the string to mkopts
 -}

parser :: String -> Exp
parser = parser_ . oTypes

-- enactor ---------------------------------------------------------------------

{- | how to take a value, perform IO on it to provide a user value.  This must
     be capable of handling default values, too (so that, say, a default filero
     of "/etc/motd" doesn't do its IO unless the default is actually used)
 -}

enactor :: String -> Exp
enactor = enactor_ . oTypes
-- enactor _ = VarE 'enactOpt

-- startIsDefault --------------------------------------------------------------

{- | If true, there are no distinct start & default; the start value is the
     default value.  E.g., for incr, it makes no sense to have different start &
     default values.
 -}
startIsDefault :: String -> Bool
startIsDefault = startIsDefault_ . oTypes
