{-# LANGUAGE MultiParamTypeClasses
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
  ( pclvType, pclvTypename, parser, setter, enactor, startIsDefault
  , typeDefault, typeStart, optionTypename, optionType )
where

-- base --------------------------------

import Data.Char   ( isUpper )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- template-haskell --------------------

import Language.Haskell.TH  ( Exp ( AppE, ConE, LitE, SigE, VarE )
                            , Lit ( IntegerL, StringL            )
                            , Type( AppT, ArrowT, ConT           )
                            , ExpQ
                            , mkName
                            )

-- fluffy ------------------------------

import Fluffy.Data.List         ( splitOn )
import Fluffy.Language.TH       ( composeE, mAppE, stringE )
import Fluffy.Language.TH.Type  ( readType )

-- this package --------------------------------------------

import Console.Getopt                   ( setval, setvalc, setvalc', setvals )
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
                                Note that this will likely be a Maybe (or list)
                                type even for non-maybe option types, since we
                                need to handle option defaults
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
                           {- | how to parse a String to generate a value; as an
                                Exp (:: String -> optionType).  This also
                                includes parsing a default value given in the
                                string to mkopts
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
--                    (AppT (AppT ArrowT (ConT ''String)) (ConT $ mkName t))
                    (AppT (AppT ArrowT (ConT ''String)) (foldr1 AppT . fmap (ConT . mkName) $ splitOn ' ' t))

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
                     , setter_         = VarE 'setvalc
                      -- we need a parser to parse a potential default value
                     , parser_         = readInt
                     , enactor_        = VarE 'return
                     , default_        = Just . LitE $ IntegerL 0
                     , start_          = Just . LitE $ IntegerL 0
                     , startIsDefault_ = True
                     }

oTypes_ "decr" = def { pclvTypename_   = "Int"
                     , optionTypename_ = "Int"
                     , setter_         = VarE 'setvalc'
                      -- we need a parser to parse a potential default value
                     , parser_         = readInt
                     , enactor_        = VarE 'return
                     , default_        = Just . LitE $ IntegerL 0
                     , start_          = Just . LitE $ IntegerL 0
                     , startIsDefault_ = True
                     }

oTypes_ "filero" = def { pclvTypename_   = "Maybe FilePath"
                       , optionTypename_ = "FileRO"
                       , setter_         = AppE (VarE 'setval) (VarE 'return)
                       , parser_         = VarE 'id
                       , enactor_        = VarE 'openFileRO
                       }

oTypes_ ('?' : '*' : t@(h :_)) 
                | isUpper h =
                  def { pclvTypename_   = "Maybe String"
                      , optionTypename_ = '?' : t
                      , setter_         = setval_as t
                      , parser_         = VarE 'id
                        -- [| maybe (return Nothing) ((fmap Just) . enactOpt) |]
                      , enactor_        = 
                          mAppE [ VarE 'maybe
                                , AppE (VarE 'return) (ConE 'Nothing)
                                , composeE (AppE (VarE 'fmap) (ConE 'Just)) 
                                                              (VarE 'enactOpt)
                                ]
                      }

                | otherwise = error $ "no such option type: '" ++ t ++ "'"

oTypes_ ('?':t) = def { pclvTypename_   = "Maybe " ++ t
                      , optionTypename_ = '?' : t
                      , setter_         = setval_as t
                      , parser_         = readParser t
                      , enactor_        = VarE 'return
                      }

oTypes_ tt@('[':t) | last t == ']' =
                 def { pclvTypename_   = tt
                      , optionTypename_ = tt
                      , setter_         = VarE 'setvals
                      , default_        = Just $ ConE '[]
                      , start_          = Just $ ConE '[]
                      }

oTypes_ [] = error "empty typestring"

-- IO types (simple,  value as String)
oTypes_ ('*' : t@(h :_)) | isUpper h =
                 def { pclvTypename_   = "Maybe String"
                     , optionTypename_ = t
                     , setter_         = setval_as t
                     , parser_         = VarE 'id
                     , enactor_        = VarE 'enactOpt
                     , default_        = Nothing
                     }

                | otherwise = error $ "no such option type: '" ++ t ++ "'"


oTypes_ t@(h:_) | isUpper h =
                 def { pclvTypename_   = "Maybe " ++ t
                      , optionTypename_ = t
                      , setter_         = setval_as t
                      , parser_         = readParser t
                      , enactor_        = VarE 'return
                      , default_        =
                          case t of
                            "String" -> Just . LitE $ StringL ""
                            "Int"    -> Just . LitE $ IntegerL 0
                            _        -> Nothing
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
pclvTypename = pclvTypename_ . oTypes

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
