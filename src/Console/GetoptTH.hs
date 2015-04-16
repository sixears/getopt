{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}  -- for class Typeish String
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

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
  ( CmdlineParseable(..), FileRO, mkopts )
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

import Control.Exception   ( Exception(..), SomeException, evaluate, try )
import Control.Monad       ( forM_, liftM, mapAndUnzipM )
import Data.List           ( partition )
import Data.Maybe          ( fromJust )
import Data.Typeable       ( Typeable )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData, force )

-- lens --------------------------------

import Control.Lens  ( (^.), _2 )

-- template-haskell --------------------

import Language.Haskell.TH         ( Dec( SigD )
                                   , Exp( AppE, ConE, CondE, DoE
                                        , ListE, LitE, TupE, VarE )
                                   , ExpQ
                                   , Lit( StringL )
                                   , Name
                                   , Pat( TupP, VarP )
                                   , Pred( ClassP )
                                   , Q
                                   , Stmt( BindS, NoBindS )
                                   , Type( AppT, ConT, ForallT, ListT, VarT )
                                   , TyVarBndr( PlainTV )
                                   , mkName, nameBase, newName, varE
                                   )
import Language.Haskell.TH.Lib     ( DecsQ, appE )
import Language.Haskell.TH.Syntax  ( lift )

-- transformers ------------------------

import Control.Monad.IO.Class             ( liftIO )
import Control.Monad.Trans.Writer.Strict  ( WriterT, runWriterT, tell )

-- fluffy ------------------------------

import Fluffy.Data.String              ( ucfirst )
import Fluffy.Language.TH              ( appTIO, assignN, composeApE, composeE
                                       , infix2E , listOfN, mAppE, mAppEQ
                                       , mkSimpleTypedFun, nameEQ, stringEQ
                                       , tsArrows, tupleL
                                       )
import Fluffy.Language.TH.Record       ( mkLensedRecord, mkLensedRecordDef )
import Fluffy.Sys.Exit                 ( exitUsage )

-- this package --------------------------------------------

import Console.Getopt.ArgArity          ( ArgArity(..), liftAA )
import Console.Getopt                   ( HelpOpts(..), Option
                                        , getopts, helpme, mkOpt, errOut )
import Console.Getopt.CmdlineParseable  ( CmdlineParseable(..), FileRO )
import Console.Getopt.OptDesc           ( OptDesc
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
         (args, opts) <- getoptsx (return . (readType \"Int\" :: String -> Int))
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

       The lensname, whether explicit or implicitly 'inherited' from the option
       names, may not begin with a capital letter (because it becomes a function
       in code)

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
           Compilation will fail if you provide a default value
           (with <default>); at that point, the use of Maybe doesn't make a
           whole lot of sense (since you would be guaranteed a Just something).
           If you really, really want a Maybe type with a default, use an
           explicit Maybe.type (which will also mean that you can specify
           "Nothing" as an option value; but conversely that you'll also need to
           specify "Just x" explitly as an option value).

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

     getoptsx_ :: [Option Getoptsx_]
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

      getoptsx_effect :: Getoptsx__ -> IO (Either [NFException] Getoptsx)
      getoptsx_effect pclv = do
        ((string_x', incr_x', handle_x'), exs) <- runWriterT $ do
                string_x    <- tryWriteF $
                                 return (((fromMaybe "") . (view s___)) pclv);
                incr_x      <- tryWriteF $ return (view incr___ pclv)
                handle_x    <- tryWriteF $
                                 openFileRO (((fromMaybe "/etc/motd")
                                              . (view handle___)) pclv)
                return (string_x, incr_x, handle_x)
        return $ if null exs
                 then Right $ Getoptsx (fromJust string_x')
                                       (fromJust incr_x')
                                       (fromJust handle_x')
                 else Left exs

    getoptsx :: (NFData a, Show a) => (String -> IO a) -> IO ([a], Getoptsx)
    getoptsx = (t2apply (checkEx . getoptsx_effect))
               . (getopts getoptsx_ (ArgSome 1 3) "filename")

  -}

  let optdescs :: [OptDesc]
      optdescs = fmap read optcfgs

      -- assign a list of options (returned by mkOpt) to a name
      -- (getoptsx_ :: [Option Getoptsx_]; getoptsx_ = [ mkOpt ... ] above)
      mkopts_ts :: Type
      mkopts_ts = AppT ListT (AppT (ConT ''Option) (pclv_typename getoptName))

      -- create a record to hold PCLVs.  This is created in one pass; and when
      -- it comes to creating the OVs record, defaults are inserted as necessary
      -- and IO is performed to produce the user-visible opts record
      -- (data Getoptsx__ = Getoptsx__ { ... } above)
      precord :: DecsQ
      precord = mkLensedRecordDef (pclv_typename getoptName)
                                  (fmap precordDefFields optdescs)
                                  [''Show]


      -- create a record to hold final values to pass back to the user;
      -- (data Getoptsx = Getoptsx { ... } above)
      record :: DecsQ
      record = mkLensedRecord (ov_typename getoptName)
                              (fmap recordFields optdescs)
                              [''Show]

  opts :: [Exp] <- sequence $ fmap mkopt optdescs ++ [ helpmeQ arity argtype ]
  -- assign a list of mkOpt calls to the chosen var
  -- (getoptsx_ :: [Option Getoptsx_];getoptsx_ = [ mkOpt ... ] above)
  let asgn_mkopts :: DecsQ
      asgn_mkopts = return [ -- getoptsx_ :: [Option Getoptsx_]
                             SigD (cfg_name getoptName) mkopts_ts
                           , -- getoptsx_ = [ mkOpt ... ]
                             assignN (cfg_name getoptName) (ListE opts)
                           ]


  concatM [ precord      -- (data Getoptsx__ = Getoptsx__ { ... } above)
          , record       -- (data Getoptsx = Getoptsx { ... }     above)
          , asgn_mkopts  -- (getoptsx_ ...                        above)
            -- (getoptsx_effect :: Getoptsx__ ->
            --                                IO (Either [NFException] Getoptsx)
            --  getoptsx_effect pclv = pclv -> do { ... }
            --  getoptsx :: (NFData a, Show a) => (String -> IO a)
            --                                 -> IO ([a], Getoptsx)
            --  getoptsx = (t2apply (checkEx . getoptsx_effect))
            --             . (getopts getoptsx_ (ArgSome 1 3) "filename")
            --  above)
          , mkGetoptTH optdescs getoptName arity argtype
          ]

--------------------------------------------------------------------------------
--                             INTERNAL FUNCTIONS                             --
--------------------------------------------------------------------------------

-- mkGetoptTH ------------------------------------------------------------------

{- | generate effector & getopts_th fn

     (getoptsx_effect :: Getoptsx__ -> IO (Either [NFException] Getoptsx)
      getoptsx_effect pclv = do
        ((string_x', incr_x', handle_x'), exs) <- runWriterT $ do
                string_x    <- tryWriteF $
                                 return (((fromMaybe "") . (view s___)) pclv);
                incr_x      <- tryWriteF $ return (view incr___ pclv)
                handle_x    <- tryWriteF $
                                 openFileRO (((fromMaybe "/etc/motd")
                                              . (view handle___)) pclv)
                return (string_x, incr_x, handle_x)
        return $ if null exs
                 then Right $ Getoptsx (fromJust string_x')
                                       (fromJust incr_x')
                                       (fromJust handle_x')
                 else Left exs

      getoptsx :: (NFData a, Show a) => (String -> IO a)
                                     -> IO ([a], Getoptsx)

      getoptsx = (t2apply (checkEx . getoptsx_effect))
                 . (getopts getoptsx_ (ArgSome 1 3) "filename")
      above)
 -}

mkGetoptTH :: [OptDesc] -- ^ options set
           -> String    -- ^ name of fn to generate (getoptsx above)
           -> ArgArity                         -- ^ arity of arguments
           -> String                           -- ^ arg type (for help text)
           -> Q [Dec]

mkGetoptTH optdescs getoptName arity argtype = do
  typeSig <- mkGetoptTHTypeSig (ov_typename getoptName)
  eff     <- mkEffector optdescs getoptName
  let getopt_th = mk_getopt_th typeSig getoptName arity argtype
  return $ eff ++ getopt_th


-- mkEffector ------------------------------------------------------------------

{- | create effector, including type sig

      getoptsx_effect pclv = do
        ((string_x', incr_x', handle_x'), exs) <- runWriterT $ do
                string_x    <- tryWriteF $
                                 return (((fromMaybe "") . (view s___)) pclv);
                incr_x      <- tryWriteF $ return (view incr___ pclv)
                handle_x    <- tryWriteF $
                                 openFileRO (((fromMaybe "/etc/motd")
                                              . (view handle___)) pclv)
                return (string_x, incr_x, handle_x)
        return $ if null exs
                 then Right $ Getoptsx (fromJust string_x')
                                       (fromJust incr_x')
                                       (fromJust handle_x')
                 else Left exs

      above)
 -}

mkEffector :: [OptDesc]                           -- ^ option field list
           -> String                              -- ^ getopt_th name
           -> Q [Dec]

mkEffector optdescs getoptName = do
  let effectorSig = tsArrows [ pclv_typename getoptName
                             , appTIO (AppT (AppT (ConT ''Either)
                                             (AppT ListT (ConT ''NFException))) (ov_typename getoptName)) ]
  pclv    <- newName "pclv" -- name of the parameter to the effector; which is
                            -- a PCLV record
  effectorBody <- mkEffectorBody optdescs getoptName pclv
  return $ mk_effector effectorSig (effect_name getoptName) pclv effectorBody

-- mk_effector -----------------------------------------------------------------

-- | create effector fn, inc. type signature
--
--   (getoptsx_effect :: Getoptsx__ -> IO (Either [NFException] Getoptsx)
--    getoptsx_effect g = do { ... }
--    above)

mk_effector :: Type    -- ^ type signature of the fn to create
            -> String  -- ^ name of the effector fn to create
            -> Name    -- ^ function parameter name
            -> Exp     -- ^ effector body
            -> [Dec]

mk_effector ts nam g = mkSimpleTypedFun ts (mkName nam) [g]

-- mk_getopt_th ----------------------------------------------------------------

-- | generated getopts-like fn
--
--   (getoptsx :: (NFData a, Show a) => (String -> IO a) -> IO ([a], Getoptsx)
--    getoptsx = (t2apply (checkEx . getoptsx_effect))
--               . (getopts getoptsx_ (ArgSome 1 3) "filename")
--    above)

mk_getopt_th :: Type    -- ^ type signature of generated fn
             -> String  -- ^ name of fn to generate (getoptsx above)
             -> ArgArity                         -- ^ arity of arguments
             -> String                           -- ^ arg type (for help text)
             -> [Dec]

mk_getopt_th sig getoptName arity argtype =
  mkSimpleTypedFun sig (mkName getoptName) [] (infix2E lhs (VarE '(.)) rhs)
    where -- (getopts getoptsx_ (ArgSome 1 3) "filename" above)
          rhs = mAppE [ VarE 'getopts, cfg_name getoptName
                      , liftAA arity, (LitE . StringL) argtype ]
          -- (t2apply (checkEx . getoptsx_effect) above)
          lhs = AppE (VarE 't2apply)
                     (composeE (VarE 'checkEx) (effect_name getoptName))

-- mkGetoptTHTypeSig -----------------------------------------------------------

{- | type signature for generated getopts-like fn, e.g.,

     > (NFData a, Show a) =>
     >  ArgArity -> String -> (String -> IO a) -> IO ([a], Getoptsx)
 -}

mkGetoptTHTypeSig :: Type -> Q Type
mkGetoptTHTypeSig t = do
  a <- newName "a"
  return .
    ForallT [PlainTV a] [ClassP ''NFData [VarT a], ClassP ''Show [VarT a]] $
    tsArrows [ -- ConT ''ArgArity
             -- , ConT ''String
             -- ,
             tsArrows [ ConT ''String , appTIO(VarT a) ]
             ,
               appTIO (tupleL [ listOfN a, t ])
             ]

-- Typeish ---------------------------------------------------------------------

-- | a type, given a String name, represented as a Name or a Type as necessary

class Typeish t where
  convert :: String -> t

instance Typeish String   where  convert = id
instance Typeish Name     where  convert = mkName
instance Typeish Type     where  convert = ConT . mkName
instance Typeish Exp      where  convert = VarE . mkName

-- ov_typename -----------------------------------------------------------------

-- | given a name (the name of the getoptth fn to create), what type name shall
--   we use for the OV record?

ov_typename :: (Typeish t) => String -> t
ov_typename = convert . ucfirst

-- pclv_typename ---------------------------------------------------------------

-- | given a name (the name of the getoptth fn to create), what type name shall
--   we use for the PCLV record?

pclv_typename :: (Typeish t) => String -> t
pclv_typename = convert . (++ "__" ) . ov_typename

-- effect_name -----------------------------------------------------------------

-- | given a name (the name of the getoptth fn to create), what name shall we
--   use for the function from PCLV Record to OV Record?

effect_name :: (Typeish t) => String -> t
effect_name = convert . (++ "_effect")

-- cfg_name --------------------------------------------------------------------

-- | name of variable to hold the list of calls to mkOpt (getoptsx_ above)
cfg_name :: (Typeish t) => String -> t
cfg_name  = convert . (++ "_")

-- concatM ---------------------------------------------------------------------

concatM :: Monad m => [m [a]] -> m [a]
concatM = liftM concat . sequence

-- mkopt -----------------------------------------------------------------------

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
     "" "" -- option typename; dflt
   |]

-- mkEffectorBody --------------------------------------------------------------

{- | build a (do) stmt that takes a GetoptName__ record, for each field in turn
     extracts the PCLV, passes through the relevant defaulter, to the relevant
     enactor; and ultimately builds a GetoptName record from the resultant values

     type of the resulting expression is GetoptName__ -> IO GetoptName
-}

-- mkEffectorBody g = do
--   ((a, b, ...), exs) <- runWriterT $ do
--                             a <- tryWriteF $ enactor (dfGetter a___)
--                             b <- tryWriteF $ enactor (dfGetter b___)
--                             ...
--                             return (a, b, ...)
--   return $ if null exs
--            then Right $ GetoptName (fromJust a) (fromJust b) ...
--            else Left exs

-- (do
--   ((string_x', incr_x', handle_x'), exs) <- runWriterT $ do
--           string_x    <- tryWriteF $
--                            return (((fromMaybe "") . (view s___)) pclv);
--           incr_x      <- tryWriteF $ return (view incr___ pclv)
--           handle_x    <- tryWriteF $
--                            openFileRO (((fromMaybe "/etc/motd")
--                                         . (view handle___)) pclv)
--           return (string_x, incr_x, handle_x)
--   return $ if null exs
--            then Right $ Getoptsx (fromJust string_x')
--                                  (fromJust incr_x')
--                                  (fromJust handle_x')
--            else Left exs
--  above)

mkEffectorBody :: [OptDesc]  -- ^ option field list
               -> String     -- ^ name of the generated getopt_th
               -> Name       -- ^ name of the fn param
               -> ExpQ

mkEffectorBody optdescs getoptName pclv =  do
  -- bs are the names that are assigned to, within the writer monad
  -- binds are each individual bound defaulted option
  (bs, binds) <- mapAndUnzipM (effectBind pclv) optdescs
  -- mbs are the names of the maybe values, having been try-ed; in each case,
  -- it's the name from within the writer, plus a trailing "'"
  mbs         <- sequence $ fmap (newName . nameBase) bs
  -- rtn_bs_tup is the return of a tuple of each bind (return (a, b, ...))
  let rtn_bs_tup = [NoBindS (AppE (VarE 'return) (TupE (fmap VarE bs)))]
      run_writer = composeApE (VarE 'runWriterT) (DoE (binds ++ rtn_bs_tup))
      exs        = mkName "exs"
      -- c'tor args are the map of fromJust across the maybe binds
      c'tor_args = fmap (AppE (VarE 'fromJust) . VarE) mbs
      c'tor      = mAppE ((ConE $ ov_typename getoptName) : c'tor_args)
      check      = CondE -- if null exs
                         (AppE (VarE 'null) (VarE exs))
                         -- then Right $ GetoptName (fromJust a) (fromJust b) ...
                         (composeApE (ConE 'Right) c'tor)
                         -- else Left exs
                         (AppE (ConE 'Left) (VarE exs))
  return (DoE ( --  ((a, b, ...), exs) <- runWriterT...
                [ BindS (TupP [TupP (fmap VarP mbs), VarP exs]) run_writer ] ++
                --  return $ if ...
                [ NoBindS (composeApE (VarE 'return) check) ]))

-- effectBind ------------------------------------------------------------------

-- | given some var pclv which is of type GetoptName__, return a stmt of the
--   form b <- enactor (dfGetter pclv)
--   (e.g., string_x <- tryWriteF $ return (((fromMaybe "") . (view s___)) pclv
--    above)

effectBind :: Name -> OptDesc -> Q (Name, Stmt)
effectBind pclv o = do
  b   <- newName $ name o
  dfg <- dfGetter o
  let enact = (AppE (enactor o) (AppE dfg (VarE pclv)))
      tryW  = (composeApE (VarE 'tryWriteF) enact)
  return (b, BindS (VarP b) tryW)

-- t2apply ---------------------------------------------------------------------

-- | apply a monadic fn to the second element of a monadic pair
t2apply :: (Monad m, Functor m) => (b -> m b') -> m (a, b) -> m (a, b')
t2apply f = (>>= _2 f)

-- tryWrite --------------------------------------------------------------------

-- | evaluate an IO thing, catch any errors, write them to a WriterT

_tryWrite :: IO a -> WriterT [SomeException] IO (Maybe a)
_tryWrite io = do
  io' <- liftIO (try io >>= evaluate)
  case io' of
    Left e  -> tell [e] >> liftIO (return Nothing)
    Right r -> (liftIO . return . Just) r

-- tryWriteF -------------------------------------------------------------------

-- | tryWriteF for things susceptible to DeepSeq

tryWriteF :: NFData a => IO a -> WriterT [NFException] IO (Maybe a)
tryWriteF io = do
  io' <- liftIO (try io >>= evaluate . force)
  case io' of
    Left e  -> tell [e] >> liftIO (return Nothing)
    Right r -> (liftIO . return . Just) r

-- NFException -----------------------------------------------------------------

-- | A SomeException susceptible to DeepSeq.  Doesn't actually do anything, but
--   means that we can use it within an NFData/force context (e.g., tryWriteF)
newtype NFException = NFException SomeException
  deriving Typeable

instance Show NFException where
  show (NFException e) = show e

instance NFData NFException where

instance Exception NFException where
  toException (NFException e) = e
  fromException = Just . NFException

-- checkEx ---------------------------------------------------------------------

-- | check an Either [Exception] a; if Left, write the exceptions to stderr and
--   exitUsage

checkEx :: Exception e => IO (Either [e] a) -> IO a
checkEx ei_io = do
  ei <- ei_io
  case ei of
    Left exs -> forM_ (fmap show exs) errOut >> exitUsage
    Right r  -> return r


-- that's all, folks! ----------------------------------------------------------
