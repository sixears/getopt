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
  ( pclvType, pclvTypename, parser, setter, enactor, startIsDefault
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

-- template-haskell --------------------

import Language.Haskell.TH  ( Exp ( AppE, ConE, LitE, SigE, VarE               )
                            , Lit ( IntegerL, StringL                          )
                            , Type( AppT, ArrowT, ConT                         )
                            , ExpQ, Name
                            , mkName
                            )

-- fluffy ------------------------------

import Fluffy.Data.List         ( splitOn2 )
import Fluffy.Language.TH       ( composeApE, composeE, mAppE, stringE )
import Fluffy.Language.TH.Type  ( readType, strToT )

-- this package --------------------------------------------

import Console.Getopt                   ( setval, setvaltM
                                        , setvalcM, setvalc'M
                                        , setvalsM, setvals'M
                                        )
import Console.Getopt.CmdlineParseable  ( FileRO, enactOpt )
import Console.Getopt.ParseOpt          ( parseAs )

--------------------------------------------------------------------------------


-- | return type for oType function; info about how we handle option-target
--   pseudo-types (e.g., incr, or ?Int)

-- so we take a command-line string, parse it with parser_, set it
-- with setter_ , into the PCLV record.  And we use
-- enactor to take the PCLV field or the uber-default, and set that into the
-- OV record.

data OptType = OptType { {- | the type used for the PCLV container field.
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
                              similar to set the value in the record.  We pass
                              in a start value to initialize the lens when it
                              is called (but not beforehand, so we may
                              determine an uncalled opt)
                          -}
                       , setter_          :: Exp -- e.g., [q| setval return |]
                         {- | how to parse a String to generate a value; as an
                              Exp (:: String -> optionType).  This also
                              includes parsing a default value given in the
                              string to mkopts.
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
setval_as t = -- [q| const $ setval (parseAs t) |]
               composeApE (VarE 'const)
                          (AppE (VarE 'setval)
                                (AppE (VarE 'parseAs) (stringE t)))

-- | like setval_as, but prefixes the result with Just to use in a '?' type
setval_as_maybe :: String -> Exp
setval_as_maybe t = -- [q| const $ setval ((fmap Just) . (parseAs t)) |]
                    composeApE (VarE 'const)
                               (AppE (VarE 'setval)
                                     (composeE (AppE (VarE 'fmap) (ConE 'Just))
                                               (AppE (VarE 'parseAs)
                                                     (stringE t))))

------------------------------------------------------------

nothingE :: Exp
nothingE = ConE 'Nothing

falseE :: Exp
falseE = ConE 'False

emptyE :: Exp
emptyE = ConE '[]

justEmptyE :: Exp
justEmptyE = AppE (ConE 'Just) emptyE -- [q| Just [] |]

-- oType for a list type, i.e., "[<type>]", e.g., "[String]"

oType_Listish :: String -> Exp -> OptType
oType_Listish t f =
  OptType { pclvTypename_   = t
          , optionTypename_ = t
          , setter_         = -- [q| f (parseAs tt) |]
                              AppE f
                                   (AppE (VarE 'parseAs) (stringE t))
          , parser_         = readParser t
          , enactor_        = VarE 'return
          , default_        = Just emptyE
          , start_          = Just emptyE
          , startIsDefault_ = False
          }

                   
oType_List :: String -> OptType
oType_List t = oType_Listish t (VarE 'setvalsM)

oType_ListSplit :: String -> String -> OptType
oType_ListSplit delim t =
  oType_Listish ("[" ++ t ++ "]") (AppE (VarE 'setvals'M) (stringE delim))

------------------------------------------------------------

-- oType for incr/decr

zeroE :: Exp
zeroE = LitE $ IntegerL 0

justZeroE :: Exp
justZeroE = AppE (ConE 'Just) zeroE

oType_Counter :: Name -> OptType
oType_Counter f =
  OptType { pclvTypename_   = "Int"
          , optionTypename_ = "Int"
          , setter_         = VarE f
           -- we need a parser to parse a potential default value
          , parser_         = readInt
          , enactor_        = VarE 'return
          , default_        = Just zeroE
          , start_          = Just justZeroE
          , startIsDefault_ = True
          }

----------------------------------------

oType_FileRO :: OptType
oType_FileRO =
  OptType { pclvTypename_   = "FilePath"
          , optionTypename_ = "FileRO"
          , setter_         = -- [q| const $ setval return |]
                              composeApE (VarE 'const)
                                         (AppE (VarE 'setval) (VarE 'return))
          , parser_         = VarE 'id
          , enactor_        = VarE 'openFileRO
          , default_        = Just nothingE
          , start_          = Just nothingE
          , startIsDefault_ = True
          }

----------------------------------------

oType_Maybe :: String -> OptType
oType_Maybe t =
  OptType { pclvTypename_   = "Maybe " ++ t
          , optionTypename_ = '?' : t
          , setter_         = setval_as_maybe t
          , parser_         = readParser t
          , enactor_        = VarE 'return
          , default_        = Just nothingE
          , start_          = Just nothingE
          , startIsDefault_ = False
          }

----------------------------------------

oType_MaybeIO :: String -> OptType
oType_MaybeIO t =
  OptType { pclvTypename_   = "Maybe String"
          , optionTypename_ = '?' : t
          , setter_         = setval_as_maybe t
          , parser_         = VarE 'id
          , enactor_        =
           -- [q| maybe (return Nothing) ((fmap Just) . enactOpt) |]
              mAppE [ VarE 'maybe
                    , AppE (VarE 'return) nothingE
                    , composeE (AppE (VarE 'fmap) (ConE 'Just)) (VarE 'enactOpt)
                    ]
          , default_        = Just nothingE
          , start_          = Just nothingE
          , startIsDefault_ = False
          }

----------------------------------------

oType_IO :: String -> OptType
oType_IO t =
  OptType { pclvTypename_   = "String"
          , optionTypename_ = t
          , setter_         = setval_as t
          , parser_         = VarE 'id
          , enactor_        = VarE 'enactOpt
          , default_        = Nothing
          , start_          = Just nothingE
          , startIsDefault_ = False
          }

----------------------------------------

oType_Simple :: String -> OptType
oType_Simple t =
  OptType { pclvTypename_   = t
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
          , start_          = Just nothingE
          , startIsDefault_ = True
          }

----------------------------------------

oType_Bool :: OptType
oType_Bool =
  OptType { pclvTypename_   = "Bool"
          , optionTypename_ = "Bool"
          , setter_         = VarE 'setvaltM
          , parser_         = readParser "Bool"
          , enactor_        = VarE 'return
          , default_        = Just falseE
          -- since values are set rather than updated, it makes
          -- no sense to have a start /= default
          , start_          = Just falseE
          , startIsDefault_ = True
          }

------------------------------------------------------------

oType :: String -> OptType

oType "incr"    = oType_Counter 'setvalcM   -- incr
oType "decr"    = oType_Counter 'setvalc'M  -- decr
oType "filero"  = oType_FileRO              -- filero
oType ('?' : '*' : t@(h :_))                -- ?*TYPE    -- (maybe IO)
    | isUpper h = oType_MaybeIO t
    | otherwise = error $ "no such option type: '?*" ++ t ++ "'"
oType ('?':t)   = oType_Maybe t             -- ?TYPE     -- (maybe)
oType ('[':',':t)                           -- [,TYPE]   -- (list, split on ,)
    | last t == ']'
                = oType ("[<,>" ++ t)
oType tt@('[':'<':s)                        -- [<X>TYPE] -- (list, split on X)
    | '>' `elem` s && last s == ']'
                = let (d,t') = splitOn2 ">" s
                      t      = init t'
                   in oType_ListSplit d t
    | otherwise = error $ "no such option type: '" ++ tt ++ "'"
oType tt@('[':t)                            -- [TYPE]    -- (list)
    | and [ not (null t), (isUpper (head t) || '[' == head t) , last t == ']' ]
                = oType_List tt
oType ('*' : t@(h : _))                     -- *TYPE     -- (IO)
    | isUpper h = oType_IO t
    | otherwise = error $ "no such option type: '*" ++ t ++ "'"
oType []        = oType_Bool                -- simple boolean
oType t@(h:_)                               -- TYPE      -- simple type
    | isUpper h = oType_Simple t
    | otherwise = error $ "no such option type: '" ++ t ++ "'"

--Y oType tt@('[' : '*' : t@(h :_))
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

--X oType ('[':',':t) | last t == ']' = oType ("[<,>" ++ t)
--X oType tt@('[':'<':s) | '>' `elem` s && last s == ']' =
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


-- typeDefault -----------------------------------------------------------------

-- | uber-default value for named type
typeDefault :: String -> Maybe ExpQ
typeDefault = fmap return . default_ . oType


-- typeStart -------------------------------------------------------------------

-- | uber-default start value for named type
typeStart :: String -> Maybe ExpQ
typeStart = fmap return . start_ . oType

-- pclvTypename -------------------------------------------------------------------

-- | type to use within the PCLV record for a requested option type
--   (so Int becomes Maybe Int (to allow for default != start) for example)
pclvTypename :: String -> String
pclvTypename = ("Maybe " ++) . pclvTypename_ . oType

-- pclvType --------------------------------------------------------------------

-- | type to use within the PCLV record for a requested option type
--   (so Int becomes Maybe Int (to allow for default != start) for example)
pclvType :: String -> Type
pclvType = ConT . mkName . pclvTypename

-- optionTypename -----------------------------------------------------------------

-- | type to represent to the getopt developer for a requested option type (so
--   incr becomes Int, for example)
optionTypename :: String -> String
optionTypename = optionTypename_ . oType

-- optionType ------------------------------------------------------------------

-- | type to represent to the getopt developer for a requested option type (so
--   incr becomes Int, for example)
optionType :: String -> Type
optionType = ConT . mkName . optionTypename

-- setter ----------------------------------------------------------------------

{- | how to take a value that has been parsed by parser_, and store it into the
     PCLV record. Thus, no IO.  This one will need setval or similar to set the
     value in the record.  It expects a start value as its first argument.
 -}

setter :: String -> Exp
setter = setter_ . oType

-- parser ----------------------------------------------------------------------

{- | how to parse a String to generate a value; as an
     Exp (:: String -> optionType).  This also includes parsing a default value
     given in the string to mkopts
 -}

parser :: String -> Exp
parser = parser_ . oType

-- enactor ---------------------------------------------------------------------

{- | how to take a value, perform IO on it to provide a user value.  This must
     be capable of handling default values, too (so that, say, a default filero
     of "/etc/motd" doesn't do its IO unless the default is actually used)
 -}

enactor :: String -> Exp
enactor = enactor_ . oType
-- enactor _ = VarE 'enactOpt

-- startIsDefault --------------------------------------------------------------

{- | If true, there are no distinct start & default; the start value is the
     default value.  E.g., for incr, it makes no sense to have different start &
     default values.
 -}
startIsDefault :: String -> Bool
startIsDefault = startIsDefault_ . oType
