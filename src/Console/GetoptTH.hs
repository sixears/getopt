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
--           name (first argument to mkopts) of "foo", (we'll call this the
--           getopt_th fn) two records will be generated: "Foo" (the Option
--           Value (OV) record) and "Foo__" (the PCLV record).  The latter is
--           the set of Parsed Command-Line Values (PCLVs); these will be
--           transformed into the former by applying option defaults and then
--           any IO actions required to generate Option Values.  The function 
--           that does this shall be called the 'effector'.

-- base --------------------------------

import Control.Monad  ( liftM, mapAndUnzipM )
import Data.List      ( partition )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- lens --------------------------------

import Control.Lens  ( (^.) )

-- template-haskell --------------------

import Language.Haskell.TH         ( Dec
                                   , Exp( AppE, ConE, DoE
                                        , ListE, VarE )
                                   , ExpQ
                                   , Name
                                   , Pat( VarP )
                                   , Pred( ClassP )
                                   , Q
                                   , Stmt( BindS, NoBindS )
                                   , Type( ConT, ForallT, VarT )
                                   , TyVarBndr( PlainTV )
                                   , mkName, newName, varE
                                   )
import Language.Haskell.TH.Lib     ( DecsQ, appE )
import Language.Haskell.TH.Syntax  ( lift )

-- fluffy ------------------------------

import Fluffy.Data.String              ( ucfirst )
import Fluffy.Language.TH              ( appTIO, assign, assignN, infix2E
                                       , listOfN, mAppE, mAppEQ, mkSimpleTypedFun
                                       , nameEQ, stringEQ, tsArrows
                                       , tupleL
                                       )
import Fluffy.Language.TH.Record       ( mkLensedRecord, mkLensedRecordDef )

-- this package --------------------------------------------

import Console.Getopt           ( ArgArity(..), HelpOpts(..)
                                , getopts, helpme, mkOpt )
import Console.Getopt.OptDesc   ( OptDesc
                                , descn, dfltTxt, precordDefFields
                                , recordFields, dfGetter
                                , names, name, optSetVal
                                , summary
                                , enactor
                                , pclvTypename
                                )

--------------------------------------------------------------------------------
--                              PUBLIC INTERFACE                              --
--------------------------------------------------------------------------------

-- mkopts ----------------------------------------------------------------------

{- | primary entry point for options generation

     @
       $( mkopts "getoptsx" (ArgSome 1 3) "filename"
                 [ "s|string\>str::String#string summary"
                 , "i|int|Int::Int\<4\>#integer summary\ndefault 4"
                 , "C\>incr::incr#increment summary\nincrement int longhelp"
                 , "decr|D::decr\<6\>#decrement summary\ndecrement int longhelp"
                 , "handle::filero\</etc/motd\>#read-only file\nauto-opened"
                 ]
        )

       main :: IO ()
       main = do
         (args, opts) <- getoptsx arity "filename"
                                  (return . (readType \"Int\" :: String -> Int))
         forM_ [ "ARGS: " ++ show args, "OPTS: "  ++ show opts ] putStrLn
         putStrLn $ "s: "    ++ show (opts ^. s)
         putStrLn $ "i: "    ++ show (opts ^. i)
         putStrLn $ "incr: " ++ show (opts ^. incr)
         putStrLn $ "decr: " ++ show (opts ^. decr)
     @

     Call this within a splice, providing the name of a fn to generate, along
     with defining parameters.  That function will be generated, returning
     parsed arguments and options, having eagerly/strictly parsed them (so that
     any relevant errors are found at this time).

     The option strings are compiled.  The syntax is:

     >  {optnames}(>{lensname})?::{type}(<{default})>?#{shorthelp}(\n{longhelp})?

     * optnames

       A list of option names, separated by @|@, to be used for invocation.

         * Each name may be single character, being invoked with @-c@, or
           multi-character, being invoked with @--chars@.
         * Each name may consist of some combination of letters, numbers, and
           hyphens (@-@).  The first (or only) character must alphanumeric.
         * Each name must be unique across the set of options.
         * Option names are case-sensitive; @c@ and @C@ are distinct option
           names.

       The option name list may not be empty.

   * lensname

       The name of the lens to target with this option.  The same naming rules
       apply as for optnames, with the following exceptions:

         * hyphens (@-@) are not permitted; underscores (@_@) are,
         * the name may begin with an underscore or an alphabetic character
           (note not a number).

       If a lensname is not given, then the first of the given option names is
       used.

         * any hyphens are replaced with underscores,
         * an option name with a leading digit is preceded in the lensname with
           an underscore.

       Each lensname must be unique both within the option set and be a unique
       function name within the compilation context.

   * type

       The value type of the option.  This is the type of the value that is
       expected to be returned by the lens; internally, a different type
       (typically using an encapsulating monad, e.g., Maybe t).

       The available types are:

         * Standard haskell types, as an alphanumeric string.  Compound types
           (with spaces in the name) are not currently supported.  The following
           types have a natural default:

             * String - ""
             * Int    - 0

           For those that don't have a natural default, if no default is
           supplied in the option specification and the user doesn't invoke the
           given option; then an error will be generated at option parsing time
           - thus you effectively have mandatory options.

         * incr

           Takes no value on the cmdline; each invocation increases the option
           value.  Starting value is 0, or the default value if specified.  User
           sees an Int.

         * decr

           Takes no value on the cmdline; each invocation increases the option
           value.  Starting value is 0, or the default value if specified.  User
           sees an Int.

         * filero

           Takes the name of a file as cmdline argument; opens the file RO, and
           returns a handle to the file.  Generates an error at options-parsing
           runtime if no user value is supplied, and there is no default.

         * ?t or Maybe t

           This is like t, except that the value is wrapped in a Maybe; if no
           value is provided on the command line is provided, you get Nothing.
           DOCUMENT WHAT HAPPENS WHEN A DEFAULT VALUE IS PROVIDED.

         * [t]

   * default

     The value to return to the caller if the option is never invoked by the
     user.  The string provided is given to read.

   * shorthelp

     short summary used with --help; pref < 50 chars

   * longhelp

     long help used with --help=optname
-}

mkopts :: String                           -- ^ name of the getopts fn to
                                           --   create, e.g., "optCfg"
       -> ArgArity                         -- ^ arity of arguments
       -> String                           -- ^ arg type (for help text)
       -- we use just a big string to allow for optionality of bits
       -> [String]                         {- ^ opt configurations;
                                                config string, default value,
                                                long descripton
                                            -}
       -> DecsQ

mkopts getoptName arity argtype optcfgs = do
  {- mkopts "getoptsx" (ArgSome 1 3) "filename"
           [ "s|string::String#summary"
           , "incr|C::incr#increment summary\nincrement int longhelp"
           , "handle::filero</etc/motd>#read-only file\nauto-opened"
           ]

     generates something like

     data Getoptsx__ = Getoptsx__ { _s__      :: Maybe String
                                  , _incr__   :: Int
                                  , _handle__ :: Maybe FilePath
                                  }
       deriving Show
     instance Default Getoptsx__ where def = Getoptsx__ Nothing 0 Nothing

     s___ :: Lens' Getoptsx__ (Maybe String)
     s___      a b = fmap asn (a $ (_s___ b))
       where asn x = b { _s___      = x }

     incr___ :: Lens' Getoptsx_ Int
     incr___   a b = fmap asn (a $ (_incr___ b))
       where asn x = b {_incr___    = x }

     handle___ :: Lens' Getopts__ (Maybe FilePath)
     handle___ a b = fmap asn (a $ ( _handle___ b))
       where asn x = b { _handle___ = x }

     data Getoptsx = Getoptsx { _s :: String
                              , _incr :: Int
                              , _handle :: Handle
                              }
       deriving Show

     s :: Lens' Getoptsx String
     s      a b = fmap asn (a $ (_s b))      where asn x = b { _s      = x }

     incr :: Lens' Getoptsx Int
     incr   a b = fmap asn (a $ (_incr b))   where asn x = b { _incr   = x }

     handle :: Lens' Getoptsx Handle
     handle a b = fmap asn (a $ (_handle b)) where asn x = b { _handle = x }

     getoptsx_ = [ mkOpt "s" ["string"] (setval parseAs "String" s___)
                         "string summary" "" "Maybe String" "\"\""
                 , mkOpt "C" ["incr"]   (setvalc incr___)
                         "increment summary" "increment int longhelp" "Int" "0"

                 , mkOpt ""  ["handle"] (setval return handle___)
                         "read-only file" "auto-opened"
                         "Maybe FilePath" "GHC.Base.id \"/etc/motd\"",

                 , mkOpt "" ["help"] helpme (def { arg_arity = ArgSome 1 3
                                                 , argtype  = "filename" })
                         "this help" "Provide help text..." "" ""
                 ]

      getoptsx_effect :: Getoptsx__ -> IO Getoptsx
      getoptsx_effect g = do
        string_x    <- return (((fromMaybe "") . (view s___)) g);
        incr_x   <- return (view incr___ g);
        handle_x <- openFileRO (((fromMaybe "/etc/motd")
                                     . (view handle___)) g);
        (return $ (Getoptsx string_x incr_x handle_x))

    getoptsx :: (NFData a, Show a) => ArgArity -> String -> (String -> IO a)
                                   -> IO ([a], Getoptsx)
    getoptsx = (getopts getoptsx_) ... (t2apply getoptsx_effect)

  -}

  let typename   = ucfirst getoptName -- name of type holding ultimate values
                                      -- to pass back to user (Getoptsx above)
      typenameN  = mkName typename
      typenameT  = ConT typenameN
      typename__ = typename ++ "__"   -- name of type record holding pre-IO
                                      -- values that are then 'effected' to
                                      -- ultimate values (Getoptsx__ above)
      typename__N = mkName typename__
      typename__T = ConT typename__N
      -- the explicit "_" is weird, but without it I get duplicate definition
      -- error for getoptName
      -- name of a variable to hold the list of calls to mkOpt (getoptsx_ above)
      cfgName  = mkName (getoptName ++ "_")
      optdescs = fmap read optcfgs
      opts     = fmap mkopt optdescs ++ [ helpmeQ arity argtype ]

      -- assign a list of options (returned by mkOpt) to a name
      -- (getoptsx_ = [ mkOpt ... ] above)
      asgn_mkopts :: [Exp] -> DecsQ
      asgn_mkopts o  = return [assignN cfgName (ListE o)]

      -- create a record to hold PCLVs.  This is created in one pass; and when
      -- it comes to creating the OVs record, defaults are inserted as necessary
      -- and IO is performed to produce the user-visible opts record
      -- (data Getoptsx__ = Getoptsx__ { ... } above)
      precord :: DecsQ
      precord = mkLensedRecordDef typename__
                                  (fmap precordDefFields optdescs)
                                  [''Show]


      -- create a record to hold final values to pass back to the user;
      -- (data Getoptsx = Getoptsx { ... } above)
      record :: DecsQ
      record = mkLensedRecord typename (fmap recordFields optdescs) [''Show]

      -- the effector is a function of type GetoptNampe__ -> IO GetoptName;
      -- which is to effect values that have been /parsed/ (e.g., in the
      -- case of integers, checking /^\d+$/ (well, something more complex, but
      -- you get the idea) to values that have been /effected/ and particularly
      -- had any IO done.  This effecting step includes any required defaulting
      -- (getoptsx_effect above)
      effectName = getoptName ++ "_effect"

      -- given some var g which is of type GetoptName__, return a stmt of the
      -- form b <- enactor (dfGetter g)
      -- (e.g., string_x <- return (((fromMaybe "") . (view s___)) g above)
      effectBind :: Name -> OptDesc -> Q (Name, Stmt)
      effectBind g o = do
        b   <- newName $ name o
        dfg <- dfGetter o
        return (b, BindS (VarP b) (AppE (enactor o) (AppE dfg (VarE g))))

      assign_getopt = mkGetopt typenameT cfgName effectName typename__T optdescs effectBind typenameN getoptName

  concatM [ precord -- (data Getoptsx__ = Getoptsx__ { ... } above)
          , record  -- (data Getoptsx = Getoptsx { ... } above)
            -- assign a list of mkOpt calls to the chosen var
          , asgn_mkopts =<< sequence opts -- (getoptsx_ = [ mkOpt ... ] above)
            -- (getoptsx_effect = \g -> do { ... }
            --     :: Getoptsx__ :: IO Getoptsx
            --  getoptsx = (getopts getoptsx_) ... (t2apply getoptsx_effect)
            --  above)
          , assign_getopt
          ]

--------------------------------------------------------------------------------
--                             INTERNAL FUNCTIONS                             --
--------------------------------------------------------------------------------

mkGetopt typenameT cfgName effectName typename__T optdescs effectBind typenameN getoptName = do
  a <- newName "a"
  -- (NFData a, Show a) =>
  -- ArgArity -> String -> (String -> IO a) -> IO ([a], typename)
  let typeSig =
        ForallT [PlainTV a] [ClassP ''NFData [VarT a], ClassP ''Show [VarT a]] $
        tsArrows [ ConT ''ArgArity
                 , ConT ''String
                 , tsArrows [ ConT ''String , appTIO(VarT a) ]
                 , appTIO (tupleL [ listOfN a, typenameT ])
                 ]

      effectorSig = tsArrows [ typename__T
                             , appTIO typenameT ]

  mkEffector optdescs effectBind typenameN effectorSig effectName getoptName cfgName typenameT


mkEffector optdescs effectBind typenameN effectorSig effectName getoptName cfgName typenameT =
  mkEffector_ effectorSig cfgName effectName getoptName typenameT (mkEffectorBody optdescs effectBind typenameN)

--X mkEffector_ :: Type -> Name -> String -> Name -> String -> Type -> Exp -> Q [Dec]
mkEffector_ :: Type -> Name -> String -> String -> Type -> (Name -> ExpQ) -> Q [Dec]

mkEffector_ effectorSig cfgName effectName getoptName typenameT eff = do
  g <- newName "g"
  typeSig <- getopt_th_ts typenameT
  eff' <- eff g
  return $ mk_effector effectorSig effectName g eff' ++
           -- (getoptsx = (getopts getoptsx_) ...
           --             (t2apply getoptsx_effect)
           --  above)
           mk_getopt_th typeSig getoptName cfgName effectName

-- mk_effector -----------------------------------------------------------------

-- | create effector fn, inc. type signature
--
--   (getoptsx_effect :: Getoptsx__ -> IO Getoptsx
--    getoptsx_effect g = do { ... }
--    above)
  
mk_effector :: Type    -- ^ type signature of the fn to create  
            -> String  -- ^ name of the effector fn to create
            -> Name    -- ^ function parameter name
            -> Exp     -- ^ effector body
            -> [Dec]

mk_effector ts nam g eff =
  mkSimpleTypedFun ts (mkName nam) [g] eff

-- mk_getopt_th ----------------------------------------------------------------

-- | generated getopts-like fn
--
--   (getoptsx = (getopts getoptsx_) ... (t2apply getoptsx_effect)
--    above)

mk_getopt_th :: Type
             -> String  -- ^ name of fn to generate (getoptsx above)
             -> Name    -- ^ name of list of calls to mkOpt (getoptsx_ above)
             -> String  -- ^ name of effector fn (getopts_effect above)
             -> [Dec]

mk_getopt_th sig getoptName cfgName effectName =
  mkSimpleTypedFun sig (mkName getoptName) [] (infix2E lhs (VarE '(...)) rhs)
    where -- (getopts getoptsx_ above)
          lhs = AppE (VarE 'getopts) (VarE cfgName)
          -- (t2apply getoptsx_effect above)
          rhs = AppE (VarE 't2apply) (VarE $ mkName effectName)

-- getopt_th_ts ----------------------------------------------------------------

{- | type signature for generated getopts-like fn, e.g.,

     > (NFData a, Show a) =>
     >  ArgArity -> String -> (String -> IO a) -> IO ([a], Getoptsx)
 -}

getopt_th_ts :: Type -> Q Type
getopt_th_ts t = do
  a <- newName "a"
  return $
    ForallT [PlainTV a] [ClassP ''NFData [VarT a], ClassP ''Show [VarT a]] $
    tsArrows [ ConT ''ArgArity
             , ConT ''String
             , tsArrows [ ConT ''String , appTIO(VarT a) ]
             , appTIO (tupleL [ listOfN a, t ])
             ]

-- concatM ---------------------------------------------------------------------

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

-- helpmeQ ---------------------------------------------------------------------

-- | ExpQ variant of `helpme`

helpmeQ :: ArgArity -> String -> ExpQ
helpmeQ arity argtype =
  [| mkOpt "" [ "help" ] (helpme def { arg_arity = arity, arg_type = argtype })
     "this help"
     (concat [ "Provide help text: without an arg, produces a summary options "
             , "output; with an arg (--help=foo), then detailed help text for "
             , "that option (if any is available) will be output."
             ])
     "" "" -- opt typename; dflt
   |]

-- mkEffectorBody --------------------------------------------------------------

{- | build a (do) stmt that takes a GetoptName__ record, for each field in turn
     extracts the PCLV, passes through the relevant defaulter, to the relevant
     enactor; and ultimately builds a GetoptName record from the resultant values

     type of the resulting expression is GetoptName__ -> IO GetoptName
-}

-- mkEffectorBody g = do
--   a <- enactor (dfGetter a___)
--   b <- enactor (dfGetter b___)
--   ...
--   return $ GetoptName a b

-- (do
--   string_x <- return (((fromMaybe "") . (view s___)) g);
--   incr_x   <- return (view incr___ g);
--   handle_x <- openFileRO (((fromMaybe "/etc/motd")
--                            . (view handle___)) g);
--   (return $ (Getoptsx string_x incr_x handle_x))
--  below)

mkEffectorBody :: [OptDesc]  -- ^ option field list
               -> (Name -> OptDesc -> Q (Name, Stmt)) 
                             -- ^ optdesc enactor binder
               -> Name       -- ^ name of the type constructor to build to
               -> Name       -- ^ name of the fn param
               -> ExpQ

mkEffectorBody optdescs effectBind typenameN g =  do
  (bs, binds) <- mapAndUnzipM (effectBind g) optdescs
  let ctor     = mAppE ((ConE $ typenameN) : fmap VarE bs)
  return (DoE ( binds ++ [ NoBindS (infix2E (VarE 'return) (VarE '($)) ctor) ]))

-- ... -------------------------------------------------------------------------

-- | like (.), but for a (first) fn of 3 args rather than 1
(...) :: (a -> b -> c -> d) -> (d -> e) -> a -> b -> c -> e
(f ... g) a b c = g (f a b c)

t2apply :: Monad m => (b -> m b') -> m (a, b) -> m (a, b')
t2apply effect ab = do
  (a,b) <- ab
  b'    <- effect b
  return (a,b')

-- that's all, folks! ----------------------------------------------------------
