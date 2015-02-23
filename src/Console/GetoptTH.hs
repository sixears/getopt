{-# LANGUAGE TemplateHaskell #-}

{- |

Description : build Getopt handler from description using template haskell
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

A companion library to Console.GetOptions to allow the developer to specify
required options in a short space & time, and have template haskell generate
the necessary code.

This library uses Template Haskell to generate boilerplate options-handling
code, including records & lenses, aiming to leave options described simply &
concisely, and thus not detracting from the real business of the program.
 -}

module Console.GetoptTH
  ( mkopts )
where

-- THE PLAN: the programmer will create options using mkopts or similar.  An
--           'option' is a description of how to parse command line strings,
--           such that a set of Option Values is generated.  Given a function
--           name (first argument to mkopts) of "foo", two records will be
--           generated: "Foo" (the Option Value (OV) record) and "Foo__" (the
--           PCLV record).  The latter is the set of Parsed
--           Command-Line Values (PCLVs); these will be transformed into the
--           former by applying option defaults and then any IO actions required
--           to generate Option Values.

-- base --------------------------------

import Control.Monad  ( (=<<), liftM, mapAndUnzipM )
import Data.List      ( isInfixOf, partition )
import Debug.Trace    ( trace )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( (^.) )

-- template-haskell --------------------

import Language.Haskell.TH         ( Exp( AppE, ConE, DoE, InfixE
                                        , LamE, ListE, SigE, VarE )
                                   , ExpQ
                                   , Name
                                   , Pat( VarP )
                                   , Pred( ClassP )
                                   , Q
                                   , Stmt( BindS, NoBindS )
                                   , Type( AppT, ArrowT, ConT, ForallT, VarT )
                                   , TyVarBndr( PlainTV )
                                   , conE, mkName, newName, varT, varE
                                   )
import Language.Haskell.TH.Lib     ( DecsQ, appE )
import Language.Haskell.TH.Syntax  ( lift )

-- fluffy ------------------------------

import Fluffy.Data.List                ( splitOn )
import Fluffy.Data.String              ( ucfirst )
import Fluffy.Language.TH              ( assign, assignN, intE, listOfN
                                       , mAppE, mAppEQ
                                       , nameE, nameEQ, stringEQ, tsArrows
                                       , tupleL
                                       )
import Fluffy.Language.TH.Record       ( mkLensedRecord, mkLensedRecordDef )

-- this package --------------------------------------------

import Console.Getopt           ( ArgArity(..), HelpOpts(..)
                                , getopts, helpme, mkOpt, setval )
import Console.Getopt.OptDesc   ( OptDesc
                                , descn, dflt, dfltTxt, precordDefFields
                                , recordFields, dfGetter
                                , lensname, names, name, optSetVal, strt
                                , strtval, summary
                                , enactor
                                , pclvField, pclvTypename
                                )

--------------------------------------------------------------------------------

-- concatM -----------------------------

concatM :: Monad m => [m [a]] -> m [a]
concatM = liftM concat . sequence

-- mkopts ----------------------------------------------------------------------

mkopt :: OptDesc -> ExpQ
mkopt optdesc =
  let (shorts, longs) = partition ((1==) . length) $ optdesc ^. names
   in -- mkOpt shorts longs
      --       (optSetVal optdesc)
      --       (optdesc ^. summary)
      --       (optdesc ^. descn)
      --       (pclvTypename optdesc)
      --       (strtTxt optdesc)
      mAppEQ [ nameEQ "mkOpt"
             , appE (varE 'concat) (lift shorts) -- short options
             , lift longs                        -- long  options
             , optSetVal optdesc                 -- handler (setval*)
             , stringEQ $ optdesc ^. summary     -- summary help
             , stringEQ $ optdesc ^. descn       -- long help
             , stringEQ $ pclvTypename optdesc   -- type name text (for help)
             , stringEQ $ dfltTxt optdesc        -- default value (for help)
             ]

helpmeQ :: ArgArity -> String -> ExpQ
helpmeQ arity arg_type =
  [| mkOpt "" [ "help" ] (helpme def { arg_arity = arity, arg_type = arg_type })
     "this help"
     (concat [ "Provide help text: without an arg, produces a summary options "
             , "output; with an arg (--help=foo), then detailed help text for "
             , "that option (if any is available) will be output."
             ])
     "" "" -- opt typename; dflt
   |]

mkopts :: String                           -- ^ name of the variable to create,
                                           --   e.g., "optCfg"
       -> ArgArity                         -- ^ arity of arguments
       -> String                           -- ^ arg type (for help text)
       -- we use just a big string to allow for optionality of bits
       -> [String]                         {- ^ opt configurations;
                                                config string, default value,
                                                long descripton
                                            -}
       -> DecsQ

mkopts getoptName arity arg_type optcfgs = do
  -- mkopts "getoptsx" (ArgSome 1 3) "filename" ["s|string::String#summary"]
  -- generates something like
  --
  -- data Getoptsx__ = Getoptsx__ { _s__ :: Maybe String } deriving Show
  -- instance Default Getoptsx__ where def = Getoptsx__ Nothing
  -- s___ a b = fmap asn (a $ (_s___ b)) where asn x = b { _s___ = x }
  -- data Getoptsx = Getoptsx { _s :: String }
  -- s a b = fmap asn (a $ (_s b)) where asn x = b { _s = x }
  -- getoptsx_ = [ mkOpt "s" ["string"] setval parseAs "String" s___
  --                     "string summary" "" "Maybe String" "\"\""
  --             , mkOpt "" ["help"] helpme (def { arg_arity = ArgSome 1 3
  --                                             , arg_type  = "filename" })
  --                     "this help" "Provide help text..." "" ""
  --             ]
  -- getoptsx = (getopts getoptsx_) ... (t2apply getoptsx_effect)
  -- getoptsx_effect g = do
  --       string_x <- return (((fromMaybe "") . (view s___)) g
  --       (return $ (Getoptsx string_x))
  --   :: Getoptsx__ -> IO Getoptsx

  let typename   = ucfirst getoptName -- name of type holding ultimate values
                                      -- to pass back to user (Getoptsx above)
      typename__ = typename ++ "__"   -- name of type record holding pre-IO
                                      -- values that are then 'effected' to
                                      -- ultimate values (Getoptsx__ above)
      -- the explicit "_" is weird, but without it I get duplicate definition
      -- error for getoptName
      -- name of a variable to hold the list of calls to mkOpt (getoptsx_ above)
      cfgName  = mkName (getoptName ++ "_")
      optdescs = fmap read optcfgs
      opts     = fmap mkopt optdescs ++ [ helpmeQ arity arg_type ]
      -- assign a list of options (returned by mkOpt) to a name
      -- (getoptsx_ = [ mkOpt ... ] above)
      asgn_mkopts :: [Exp] -> DecsQ
      asgn_mkopts o  = return [assignN cfgName (ListE o)]

      -- create a record to hold parsed but not effected values; that is, in the
      -- case of non-IO values, the end result; but in the case of IO values, a
      -- non-IO pre-representation.  This is created in one pass; and when it
      -- comes to creating the record returned to the user, defaults are
      -- inserted as necessary and IO performed to produce the user-visible opts
      -- record
      -- (data Getoptsx__ = Getoptsx__ { ... } above)
      precord :: DecsQ
      precord = mkLensedRecordDef typename__
                                  (fmap precordDefFields optdescs)
                                  [''Show]


      -- create a record to hold final values to pass back to the user; 
      -- (data Getoptsx = Getoptsx { ... } above)
      record :: DecsQ
      record = mkLensedRecord typename
                                 (fmap recordFields optdescs)
                                 [''Show]

      -- the effector is a function of type GetoptName__ -> IO GetoptName;
      -- which is to effect values that have been /parsed/ (e.g., in the
      -- case of integers, checking /^\d+$/ (well, something more complex, but
      -- you get the idea) to values that have been /effected/ and particularly
      -- had any IO done.  This effecting step includes any required defaulting
      -- (getoptsx_effect above)
      effectName = getoptName ++ "_effect"

      effectBind :: Name -> OptDesc -> Q (Name, Stmt)
      effectBind g o = do
        b   <- newName $ name o
        dfg <- dfGetter o
        return (b, BindS (VarP b) (AppE (enactor o) (AppE dfg (VarE g))))

      -- effector g = do
      --   a <- updater (g ^. a___)
      --   b <- updater (g ^. b___)
      --   ...
      --   return $ GetoptName__ a b c
      effector = do
        let o = head optdescs
        g <- newName "g"
        a <- newName (name o)
        dfg <- dfGetter o
        (bs, binds) <- mapAndUnzipM (effectBind g) optdescs
        return $
          LamE [ VarP g ] (DoE ( binds ++
                                [ NoBindS (InfixE (Just (VarE 'return))
                                                 (VarE '($))
                                                 (Just (mAppE ((ConE $ mkName typename) : fmap VarE bs)))
                                         )
                                                                                                                  ]))

      -- getoptName = Fluffy.Console.Getopt.getopts getoptName_
      -- getoptName_effect = effector :: GetoptName__ -> IO GetoptName

      assign_getopt = do
        a <- newName "a"
        -- (Show a) => ArgArity -> String -> (String -> IO a)
        --          -> IO ([a], typename)
        let typeSig = tsArrows [ ConT ''ArgArity
                               , ConT ''String
                               , tsArrows [ ConT ''String
                                          , AppT (ConT ''IO) (VarT a) ]
                               , AppT (ConT ''IO)
                                      (tupleL [ listOfN a
                                              , ConT $ mkName typename ])
                               ]

            lhs = AppE (VarE 'getopts) (VarE cfgName)
            rhs = AppE (VarE 't2apply) (VarE $ mkName effectName)

        effector >>= \c ->
          return [ -- assign getoptName lhs --  (AppE (VarE 'getopts) (VarE cfgName))
                   -- getoptsx = (getopts getoptsx_) ... (t2apply getoptsx_effect)
                   assign getoptName (InfixE (Just lhs) (VarE '(...)) (Just rhs))
  -- getoptsx_effect g = do
  --       string_x <- return (((fromMaybe "") . (view s___)) g
  --       (return $ (Getoptsx string_x))
  --   :: Getoptsx__ -> IO Getoptsx
                 , assign effectName
                     (SigE c (tsArrows [ ConT $ mkName typename__
                                       , AppT (ConT ''IO)
                                              (ConT $ mkName typename)
                                       ]))
                 ]

  concatM [ precord, record
          -- assign a list of mkOpt calls to the chosen var
          , asgn_mkopts =<< sequence opts
          , assign_getopt
          ]

-- | like (.), but for a (first) fn of 3 args rather than 1
(...) :: (a -> b -> c -> d) -> (d -> e) -> a -> b -> c -> e
(f ... g) a b c = g (f a b c)

t2apply :: Monad m => (b -> m b') -> m (a, b) -> m (a, b')
t2apply effect ab = do
  (a,b) <- ab
  b'    <- effect b
  return (a,b')


-- that's all, folks! ----------------------------------------------------------
