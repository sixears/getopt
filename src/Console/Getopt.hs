{-# LANGUAGE FlexibleInstances
           , OverlappingInstances
           , RankNTypes
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeSynonymInstances
 #-}
{-# OPTIONS_HADDOCK show-extensions #-}


{- |

Description : command-line option handling
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

A library for handling cmdline options & arguments, including checking for
argument count, parsing arguments & options, pre-evaluated IO on both (to
trap errors early), handling collections of options, and automatically
generating help texts.

 -}

module Console.Getopt
  ( -- I tried to use the '-- $name' syntax here, but got a 'parse error' on the
    -- next line of non-doc haskell (even using the example c'n'p from the
    -- manual); so I gave up

    -- * Synopsis

    {- | To use this module:

             (0) create a (lensed) record that will store the result
                 of parsing the options (and give it a default value, if you're
                 going to use getopts*)
             (0) create an options-parsing configuration (which is just a
                 list of individual parsers), and
             (0) call one of the getopts* family (or getOptions).

         Here is a simple example.

         Please note the distinction between /arguments/, which are the standard
         textual values given after the program name; and the /options/, which
         are optional tweaks given using '--foo' or '-F'; thus in @grep
         -r --include \\*.hs hlib GetOptions@, the @hlib@ and @GetOptions@ are
         considered arguments, while the @-r@ and the @--include \\*.hs@ are
         considered options.

         @
         {-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
         import Control.Lens  ( (^.), makeLenses )
         import Data.Default  ( Default( def ) )
         import Data.List     ( intercalate )

         import Fluffy.Console.Getopt  ( ArgArity( ArgSome ), Option
                                       , getopts, mkOpt, setval )

         -- opts & options ----------------------

         data Opts = Opts { _str :: Maybe String }
         $( makeLenses ''Opts )

         instance Default Opts where def = Opts Nothing

         optCfg :: [Option Opts]
         optCfg = [ mkOpt "s"  [ "str"  ] (setval return str)
                          "summary" "option description" "String" ""
                  ]

         -- main --------------------------------

         main :: IO ()
         main = do
           (args :: [Int], opts) <- getopts optCfg (ArgSome 1 3) "integer"
                                            (return . read)
           let s = maybe "" (++ " ") (opts ^. str)
           putStrLn $ s ++ unwords (map show args)
         @

         Line by line; we start with:

         >    data Opts = Opts { _str :: Maybe String }

         this defines a record in which to store the results of parsing.  In
         this case, the single available option is stored as a string value; the
         @Maybe@ allows for the situation that the option wasn't invoked.

         >    $(makeLenses ''Opts )

         Each setval* function expects a Lens' argument to work with.  You can
         use this library without lenses - @Lens' s a@ is just a function - but
         the lens package makes it all much simpler.

         @
         type Lens' s a = 
           &#8704; (f :: * -> *) . Functor f => (a -> f a) -> s -> f s
         @

         A default instance of your Opts class is required so that
         getopts/getopts' has some values to start with.  Calls to
         getOptions/getopts_ don't need this.

         >    instance Default Opts where def = Opts Nothing

         Now configure your options parsing.  A configuration is just a list of
         individual options, each being built with mkOpt.  Note that the final
         four string arguments are only used by options constructed with helpme*;
         if you don't use that (as in this example), you don't need them.

         >    optCfg :: [Option Opts]
         >    optCfg = [ mkOpt "s"  [ "str"  ] (setval return str)
         >                     "summary" "option description" "String" ""
         >             ]

         Call getopts, parsing the configuration, a definition (@ArgArity@) of
         how many arguments you need, a text description of those arguments
         (again, for use with a helpme* option, but also when generating error
         messages), and a rule for parsing those arguments.  The type
         specification on @args@ is required because of the polymorphism of
         @read@.

         >    main :: IO ()
         >    main = do
         >      (args :: [Int], opts) <- getopts optCfg (ArgSome 1 3) "integer"
         >                                       (return . read)
         >      let s = maybe "" (++ " ") (opts ^. str)
         >      putStrLn $ s ++ unwords (map show args)

     -}

    -- * Notes

    {- | All option names are treated case-insensitively -}

    {- | This library eagerly evaluates argument an option parses when an entry
         point is called.  That is a deliberate strategy to ensure that once
         the program proper begins, all the options/arguments have been passed
         and are valid.  The rationale is that it is not good form to spend 30
         minutes crunching data only to then declare that "seven" is not a valid
         integer, or whatever.
     -}

    -- * Primary Entry Points
    getopts, getopts', getopts_, getOptions

    -- * Types
    -- OptParse isn't really needed to be exported, but exporting it means it
    -- gets documented
  , ArgArity(..), HelpOpts(..), Option, OptParse, ParseState, ParseState_

  -- * Option Constructors
  , helpme, helpme', mkOpt, mkOptsB

  -- * setval* Variants
  , setvalOW, setval, setvalc, setvalc', setvalm, setvalm_, setvals
  , setvals', setvalt, setvalf, setval', setvalAList, setvalAList'

  -- * useful extras
  , NFHandle( NFHandle ), progName, unhandle
  )
where

-- remove use of arity, argtype name, from getopts call generated by mkopts
--   since they're passed to mkopts itself
-- option invocation names should accept '-' not '_'.  Lensname should tr/-/_/.
--   starting with '-' or numbers should be illegal (numbers because you can't
--   have functions starting with numbers)
-- add tests for
--   * lensname beginning with _
--   * lensname defaulted from optname, with - replaced by _
--   * lensname defaulted from optname, with leading digit preceded by '_'
-- clean build from scratch
-- add tests to T/Getopt.hs / getopt-ex for setvalm{,'}; improve doc for
--   setvalm{,'}
-- add tests that check that an option with no default causes a throw at option-
--   parsing time if the user doesn't supply an option; test this for filero
--   when used without a default value
-- mandatory option; leading '!'; causes error if this option isn't invoked.
--   particularly required for, e.g., String, which has a natural default and so
--   wouldn't get the benefit of default-less checking.  Or should we use
--   encourage the use of a newtype for these?  One consideration is if one uses
--   say Float, which later gets given a natural default - action-at-a-distance.
--   Perhaps therefore we should require a leading '!' for non-default,
--   non-maybe options
-- subsidiary value tests; e.g., for (strictly) positive ints - is that _check?
-- add readInt, etc. to Language/TH/Type.hs
-- allow the same lens to be specified in multiple fields - if the types match,
--   and there is no more than one default value.  Thus incr, decr could refer to
--   the same option slot
-- common setInt, setInts, setStrs, etc., for common types; int float string bool
--   (simple turn-it-on) Bool ( True|False) fnexists fnopen fncouldwrite
--   dir
-- maps using x=y (or some other delimiter, or two arguments)
--   two versions; one that blows up on repeated keys, one that produces
--     -- current version sees current set of keys&values; so setval wrapper fns
--     -- could provide blowup as necessary
-- std hooks in TH version for help, verbose/quiet, dry-run
-- ability to mix TH setup with standard stuff (so we can add complex options on
--   later)
-- remove debug
-- TH maybe type that handles defaulted value (provides maybe fn to access lens)
--   that is, lens is of type Maybe b, but client accessor uses maybe dflt id
--   to be an accessor of type b

{-

setValue'          -- optname, mb_optval, args, old_value
 |
 +-setValue        -- None|Maybe|One - optname, mb_optval, old_value
 |  |
 |  +-setvalt      -- Bool; set to true
 |  |
 |  +-setvalf      -- Bool; set to false
 |  |
 |  +-setvalc      -- Int; increment
 |  |
 |  +-setvalc'     -- Int; decrement
 |  |
 |  +-setval'      -- Single value setter; String -> b -> IO b
 |  |  |
 |  |  +-setvalOW  -- :: b ; overwrites prior value; String -> IO b
 |  |  |
 |  |  +-setval    -- :: Maybe b ; error on multiple call; String -> IO b
 |  |  |
 |  |  +-setvals   -- :: [b] ; appends each new value; String -> IO b
 |  |  |
 |  |  +-setvals'  -- :: [b] ; appends each new value; split on delimiter;
 |  |                          String, String -> IO b
 |  +-setvalm      -- :: b ; allows -o=foo but not -o foo; overwrites
 |    |                      Maybe String -> b -> IO b
 |    +-setvalm_   -- :: Bool ; specialization of setvalm for Bool values
 |
 +-setvalAList'    -- :: [(k,v)] ; looks for keys & values, appends to list
    |                              takes a string delimiter to split k/v
    |                              in -o=foo=>bar or -o foo=>bar
    +-setvalAList  -- :: [(k,v)] ; setvalAList with '=' delimiter

-}

-- base --------------------------------

import Control.Exception  ( SomeException, evaluate, try )
import Control.Monad      ( foldM, forM_, unless, when )
import Data.Char          ( isAlphaNum )
import Data.Either        ( partitionEithers )
import Data.List          ( intercalate, partition )
import Data.Maybe         ( catMaybes, fromJust )
import System.Environment ( getArgs, getProgName )
import System.IO          ( Handle )
import System.IO.Unsafe   ( unsafeDupablePerformIO )
import Text.Printf        ( printf )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData, force )

-- lens --------------------------------

import Control.Lens  ( (&), (^.), (.~), (%~), Lens', makeLenses, view )

-- local packages ------------------------------------------

-- text-block --------------------------

import Text.Block  ( Block, TableOptions(..), WrapOptions(..)
                   , VAlign( VTop )
                   , hjoin, mkBlock, strings, stripEnds
                   , table'
                   , wrap, wrap'
                   )


-- Fluffy ------------------------------

import Fluffy.Control.Lens      ( (++=), (=++), (~:~) )
import Fluffy.Data.AList        ( alist_dups )
import Fluffy.Data.List         ( spanEnds, splitOn2, splitOnL, tr1 )
import Fluffy.Data.String       ( lc, unlines' )
import Fluffy.Sys.Exit          ( exitUtility, exitUsage )
import Fluffy.Sys.IO            ( ePutStrLn )

-- this package ------------------------

import Console.Getopt.ArgArity  ( ArgArity(..), check_arity, show_arity )

--------------------------------------------------------------------------------
-- ParseState
--------------------------------------------------------------------------------

-- | the parsed accumulation so far; the options found, the args found, errors &
--   helps encountered.  Also the remaining strings to parse, and the options
--   configuration

-- parameterization required to handle the mutually recursive types
-- ParseState <-> Option

data ParseState_ a o =
  ParseState_ { _strs  :: [String] -- ^ strings left to parse
              , _args  :: [String] -- ^ args collected so far
              , _opts  :: o        -- ^ option values collected so far
              , _errs  :: [String] -- ^ errors collected so far
              , _helps :: [(String,String)] {- ^ help strings collected so
                                                 far as an alist to avoid
                                                 reprinting multiple help
                                                 requests
                                             -}
              , _cfg   ::  a       -- ^ the options configuration
              }

$( makeLenses ''ParseState_ )

-- | drop the first n values from strs (typically because they have now been
--   parsed); ASSUMES n <= length strs
tl :: Int -> ParseState_ a o -> ParseState_ a o
tl n state = state & strs %~ drop n

-- | drop the first value from strs (typically because it has now been
--   parsed); ASSUMES 1 <= length strs
tl1 :: ParseState_ a o -> ParseState_ a o
tl1 = tl 1

-- | add a help text for a given key string to the parse state (for outputting).
--   We use key strings to avoid repeating help texts
sethelp :: ParseState o -> String -> String -> ParseState o
sethelp state key val = state & helps %~ (\h -> case lookup key h of
                                                  Just _ -> h
                                                  Nothing -> h ++ [(key,val)])

-- | a ParseState_ over Option
type ParseState o = ParseState_ [Option o] o

-- | an initial ParseState for a given argv using a given cfg
newparse :: o -> [String] -> [Option o] -> ParseState o
newparse os argv = ParseState_ argv [] os [] []

--------------------------------------------------------------------------------
-- Option
--------------------------------------------------------------------------------

-- | first string is the name of the option as invoked; Maybe String is the string
-- after '=' if --option=string is used

type OptParse o = String -> Maybe String -> ParseState o -> IO (ParseState o)

-- | a generic cmdline option descriptor, use mkOpt or mkOptsB to create; o is
--   the option value accumulator (which must be a lensed record)

data Option o = Option { _shortnames :: [Char]   -- ^ list of single-char (-x)
                                                 --   invocation names
                       , _longnames  :: [String] -- ^ list of longer (--foo)
                                                 --   invocation names
                       {- | parse a single option.  May consume values from the
                            ParseState strs if appropriate (typically one off
                            the front).  Returns a new ParseState with options
                            (or errors, or helps) set in accordance with the
                            option invoked.  Uses IO to allow for checking for
                            file existence, etc.
                       -}
                       , _parse      :: OptParse o
                       {- | Brief help text for outputting with summary help.
                            Typically, around 45 chars would avoid wrapping
                        -}
                       , _summary    :: String
                       , _desc       :: String   -- ^ Longer descriptive text,
                                                 -- to be output with --help=foo
                                                 -- or similar
                       , _typename   :: String -- ^ option type name for help
                                               --   text
                       , _dflt       :: String -- ^ option default value for
                                               --   help text
                       }

$( makeLenses ''Option )

instance Show (Option o) where
  show o = "Option: " ++ intercalate ", " [ show(o ^. shortnames)
                                          , show(o ^. longnames)
                                          , o ^. summary
                                          , o ^. desc
                                          , o ^. typename
                                          , o ^. dflt
                                          ]

instance Eq (Option o) where
  a == b = show a == show b

instance Ord (Option a) where
  a `compare` b = show a `compare` show b

-- mkOpt -------------------------------

-- | create a new Option value, with error checking
--
--   This example parses file names and passes back filehandles opened in
--   read-only mode (and errors if the RO open fails).  The use of @setvals@
--   matched with a lens on a list type means that the option may be called
--   multiple times, with the result of each call being accumulated in the
--   lensed list.
--
-- > mkOpt "h"  [ "handle"  ] (setvals (\x -> openFile x ReadMode) handles)
-- >                          "handle" "Handle" "[Handle]" "[]"

mkOpt :: [Char]     -- ^ list of single-char options
      -> [String]   -- ^ list of multi-char options
      -> OptParse o -- ^ handle this option (see setval*)
      -> String     -- ^ summary (pref < 50 chars) help  (used with --help)
      -> String     -- ^ long help (for --help=optname)  (used with --help)
      -> String     -- ^ type name text  (used with --help)
      -> String     -- ^ default value text  (used with --help)
      -> Option o
mkOpt ss ls p s d t f =
  let es    =    fmap err_invsname (filter (not . validShortOptName) ss)
              ++ fmap err_invlname (filter (not . validLongOptName)  ls)
   in case es of
        [] -> Option ss ls p s d t f
        _  -> error $ intercalate "\n" es

-- mkOptsB -----------------------------

-- | make --bool & --no-bool options for +ve & -ve setting options
--   any short options are treated as +ve (set to True) only
--
--   This example creates options @-f@, @--foo@, @--no-foo@; @-f@ and @--foo@
--   set the value of the foo lens to Just True; @--no-foo@ sets the value of
--   the foo lens to Just False.  This uses a @Maybe@ type so the program may
--   distinguish 'no setting given'.
--
-- > mkOptsB "f" [ "foo" ] foo
-- >                          "summary" "long desc" "Bool"  "Nothing"

mkOptsB :: [Char]               -- ^ list of single-char options
        -> [String]             -- ^ list of multi-char options
        -> Lens' o (Maybe Bool) -- ^ handle this option (see setval*)
        -> String               -- ^ summary (pref < 50 chars) help
        -> String               -- ^ long help (for --help=optname)
        -> String               -- ^ type name text
        -> String               -- ^ default value text
        -> [Option o]
mkOptsB ss ls l s d t f = let o = mkOpt ss ls (setvalm_ l) s d t f
                           in [ o
                              , mkOpt "" (fmap ("no-" ++) ls) (setvalm__ l)
                                ("logical inversion of " ++ optionFullName o)
                                ("logical inversion of " ++ optionFullName o)
                                t f
                              ]

-- nameMatch ---------------------------

-- | does the given name (found on the cmdline, sans any leading hyphens)
--   match this option?
nameMatch :: String -> Option o -> Bool
nameMatch s o = case s of
                  a : [] -> a `elem` (o ^. shortnames)
                  _      -> s `elem` (o ^. longnames)

-- optionName --------------------------

-- | simple, short name to use to refer to the option
optionName :: Option o -> String
optionName o = case o ^. longnames of
                 [] -> [head $ o ^. shortnames]
                 n : _ -> n

-- optionNames -------------------------

-- | all of the available cmdline option invocation names (including leading
--   hyphens)
optionNames :: Option o -> [String]
optionNames o =    fmap (\ c -> ['-', c]) (o ^. shortnames)
                ++ fmap ("--" ++) (o ^. longnames)

-- optionFullName ----------------------

-- | full name of an option

optionFullName :: Option o -> String
optionFullName = intercalate "|" . optionNames

-- noNames -----------------------------

-- | predicate; is this option lacking any names?

noNames :: Option o -> Bool
noNames = null . optionNames

-- validShortOptName -------------------

-- | is this name a valid short option name?

validShortOptName :: Char -> Bool
validShortOptName = isAlphaNum

-- validLongOptName --------------------

-- | is this name a valid long option name?
validLongOptName :: String -> Bool
validLongOptName o = let valid_char c = -- c is either alphanum, or '-'
                                        any ($ c) [ isAlphaNum, (== '-') ]
                     in    (1 < length o)
                        && isAlphaNum (head o)
                        && all valid_char o
--------------------------------------------------------------------------------
-- ValSemantic
--------------------------------------------------------------------------------

{- | is a value taken for this setValue?  ValOpt will allow an o=foo style
     val but not -o foo style.  In the case of an option that takes no
     value, or allows an optional value but none is given, then the parser
     will be called with None.  In all other cases, Just s will be passed.
 -}
data ValSemantic = ValNone | ValOpt | ValMandatory
  deriving Eq

--------------------------------------------------------------------------------
-- errors
--------------------------------------------------------------------------------

err_combine :: String -> String
err_combine =
  printf "may not combine clustered single-char options with a value '%s'"

err_invname :: Char -> String
err_invname = printf "invalid option name '%c'"

err_nonopt :: String -> String
err_nonopt = printf "no such option: '%s'"

err_nolonghelp :: String -> String
err_nolonghelp = printf "no long help available for option '%s'"

err_noarg :: String -> String
err_noarg = printf "option '%s' brooks no argument"

err_reqval :: String -> String
err_reqval = printf "option '%s' requires a value"

err_invlname :: String -> String
err_invlname   = printf "not a valid long option name: '%s'"

err_invsname :: Char -> String
err_invsname = printf "not a valid short option name: '%c'"

err_badval :: (Show e) => Maybe String -> e -> String -> String
err_badval v e o  =
  case v of
    Just v' ->
      printf "failed to parse value '%s' for option '%s':\n  %s" v' o (show e)
    Nothing ->
      printf "failed to enact option '%s':\n  %s" o (show e)

err_ambgopt :: String -> String
err_ambgopt  = printf "ambiguous option name: '%s'"

err_noname :: Show o => o -> String
err_noname o = printf "option has no name:\n  %s" (show o)

err_multsopt :: Show a => Char -> [a] -> String
err_multsopt s os =
     printf "option short name '%c' is used for multiple options:" s
  ++ (("\n  " ++) =<< fmap show os)

err_multlopt :: Show a => String -> [a] -> String
err_multlopt l os =
     printf "option long name '%s' is used for multiple options:" l
  ++ (("\n  " ++) =<< fmap show os)

err_badkey :: String -> String -> SomeException -> String -> String
err_badkey k v e o =
  printf "failed to parse key '%s' for option '%s' (value '%s'):\n  %s"
         k o v (show e)

err_badkval :: String -> String -> SomeException -> String -> String
err_badkval k v e o =
  printf "failed to parse value '%s' for option '%s' key '%s':\n  %s"
         v o k (show e)

err_reqkval :: String -> String -> String
err_reqkval = printf "key '%s' for option '%s' requires a value"

err_reqkeyval :: String -> String
err_reqkeyval = printf "option '%s' requires a key & a value"

err_kv_nodelim :: String -> String -> String
err_kv_nodelim = printf "option '%s' does not permit postfix assignment (%s)"

err_missing_kvdelim :: String -> String -> String -> String
err_missing_kvdelim =
  printf "option '%s' assignment '%s' is missing delimiter '%s'"

err_already_set :: String -> String
err_already_set = printf "option already set to '%s'"

--------------------------------------------------------------------------------
--                           PRIMARY ENTRY POINTS                             --
--------------------------------------------------------------------------------

-- getOptions --------------------------


{- | Parse a list of strings per a configuration, returning args, options,
     errors & help strings.  Will error out if the options config contains two
     or more distinct options utilizing the same short or long names, or any
     options with no names at all.  Note that 'distinct' here doesn't include
     the parser fn, since functions aren't generally comparable (instances of
     Eq); so two options that are equal in everything except for their parser
     will *not* generate an error.
 -}

getOptions :: o                                  -- ^ initial value of o, the
                                                 --   option collection type
           -> [Option o]                         -- ^ option parsing config
           -> [String]                           -- ^ list of strings to parse
           -> IO ([String],o,[String],[String])  -- ^ arguments, options,
                                                 --   errors & help strings

getOptions start optCfg argv = do
  let sopts = (\ o -> fmap (\ s -> (s, o)) (o ^. shortnames)) =<< optCfg
      lopts = (\ o -> fmap (\ l -> (l, o)) (o ^. longnames)) =<< optCfg

      -- | find the dup keys in an alist, produce a set of error strings for the
      --   dups
      find_dups_e :: (Ord k, Ord v)
                  => (k -> [v] -> String) -- ^ error function
                  -> [(k, v)]
                  -> [String]
      find_dups_e e ls = fmap (uncurry e) $ alist_dups ls

      -- | blow up if cfg specifies multiple options using the same shortname
      --   or longname, or option has no name at all
      es = concat [ fmap err_noname (filter noNames optCfg)
                  , find_dups_e err_multsopt sopts
                  , find_dups_e err_multlopt lopts
                  ]

  unless (null es) . error $ intercalate "\n" es
  ps <- parse_options (newparse start argv optCfg)
  return (reverse (ps ^. args), ps ^. opts,
          reverse (ps ^. errs), fmap snd (ps ^. helps))

-- getopts_ ----------------------------

-- | parse options & arguments from a list of strings, checking for the required
--   number of arguments
getopts_ :: (NFData a, Show a)
          => [Option o] -> ArgArity -> String -> (String -> IO a) -> o -> [String]
         -> IO ([a], o)
getopts_ cfg' arity argtype parser start cmdline_args  = do
  let -- prefix errs lines with leading "! " (and subsequent lines with "  ")
      eprefix :: [String] -> [String]
      eprefix  = fmap $ \e -> case lines e of
                                (h:t) -> unlines' (("! "++h) : fmap ("  "++) t)
                                []    -> ""
      -- ensure trailing "\n"
      lp        = fmap (unlines . lines)
      -- parse a with parser, strictly, and return either an error or the parsed
      -- value
      -- parser' :: String -> IO (Either String a)
      parser' a = do
        p <- try $ parser a >>= evaluate . force
        case p of
          Left (e :: SomeException) ->
            return . Left $ concat [ "failed to parse arg '", a, "' as ", argtype
                                   , ":\n  ", show e ]
          Right a' -> return $ Right a'

  (args_, opts_, errs_, helps_) <- getOptions start cfg' cmdline_args
  -- check for and warn of errors; exit if errors found
  forM_ (eprefix errs_) ePutStrLn
  forM_ (lines (intercalate "--------\n" (lp helps_))) putStrLn
  when (0 < length errs_)  exitUsage
  when (0 < length helps_) exitUtility
  case check_arity arity args_ of
    Just e -> do
      ePutStrLn $ "! " ++ e
      exitUtility
    Nothing -> return ()

  -- parse the args, check for errors, maybe warn & exit
  as <- mapM parser' args_ >>= evaluate . force
  let (parse_errs, args_') = partitionEithers as
  when (0 < length parse_errs)
       (forM_ (eprefix parse_errs) ePutStrLn >> exitUsage)
  return (args_', opts_)

-- getopts -----------------------------

-- | parse options & arguments from the command line, checking for the required
--   number of arguments

getopts :: (Default o, NFData a, Show a)
        => [Option o]       -- ^ option parsing configuration
        -> ArgArity         -- ^ how many cmdline arguments to require
        -> String           -- ^ "type" of cmdline arguments (for help text)
        -> (String -> IO a) -- ^ how to parse cmdline arguments
        -> IO ([a], o)      -- ^ list of cmdline arguments, and options
getopts optCfg arity argtype parser =
  getArgs >>= getopts_ optCfg arity argtype parser def

-- getopts' ----------------------------

-- | simple defaulted getopts, accepts any number of string arguments

getopts' :: Default o => [Option o] -> IO ([String], o)
getopts' optCfg = getopts optCfg ArgAny "String" (return . id)


--------------------------------------------------------------------------------
-- setval*
--------------------------------------------------------------------------------

-- setvalOW --------------------------------------------------------------------

-- | 'setval: overwrite'; set an option value using a parser, ignoring any prior
--   existing value

setvalOW :: (NFData b) => (String -> IO b) -> Lens' o b -> OptParse o
setvalOW f = setval' (const . f)

-- setval ----------------------------------------------------------------------

-- | set a value, but only once; error on subsequent calls (that is, if the
--   pre-existing value of the lens is not Nothing)

setval :: (NFData b, Show b) => (String -> IO b) -> Lens' o (Maybe b)
                              -> OptParse o
setval f = setval' (\s o -> case o of
                               Nothing -> fmap Just (f s) -- liftM == fmap
                               Just o' -> error $ err_already_set (show o')
                   )

-- setvalc ---------------------------------------------------------------------

-- | setval for counter values; increments on each call

setvalc :: Lens' o Int -> OptParse o
setvalc = setValue ValNone (\ _ _ c -> return $ c + 1)

-- | setval for counter values; decrements on each call

setvalc' :: Lens' o Int -> OptParse o
setvalc' = setValue ValNone (\ _ _ c -> return $ c - 1)

-- setvals ---------------------------------------------------------------------

-- | setValue for many-valued args, thus appending each new value to a list

setvals :: (NFData b) => (String -> IO b) -> Lens' o [b] -> OptParse o
setvals f = setval' (\ i is -> f i >>= \ i' -> return $ is ++ [i'])

-- setvals' --------------------------------------------------------------------

-- | like setvals, but takes a delimiter to split individual arg strings (e.g.,
--   a comma)

setvals' :: (NFData b)
         => String                     -- ^ delimiter to split incoming strings
         -> (String -> IO b)           -- ^ how to parse individual values
         -> Lens' o [b] -> OptParse o
setvals' s f =
  setval' (\ i is -> mapM f (splitOnL s i) >>= \i' -> return $ is ++ i')

-- setval' ---------------------------------------------------------------------

-- | general-purpose setvalue for setting a value

setval' :: (NFData b) => (String -> b -> IO b) -- ^ parser (b is prior val)
                      -> Lens' o b             -- ^ lens to update with parsed
                                               --   value(s)
                      -> OptParse o            -- ^ optname, mb_optval, state
setval' f = setValue ValMandatory (ignoreFirst f')
              where f' m = f (fromJust m)

-- setvalm ---------------------------------------------------------------------

-- | setvalm'; subsequent calls to this option will fail (specifically, if the
--   target of the Lens has a non-Nothing value, the call will fail)

setvalm :: (NFData b, Show b)
        => (Maybe String -> IO b)
        -> Lens' o (Maybe b)
        -> OptParse o
setvalm f = setvalm' (\s oldv -> case oldv of
                                   Nothing -> fmap Just (f s)
                                   Just o  -> error $ err_already_set (show o)
                     )

-- | Set value, will handle a specified value as -o=value, but not -o value.
--   Thus a missing value (-o) (Nothing as first arg to the parser) will
--   generate either a default value or an error.

setvalm' :: (NFData b)
         => (Maybe String -> b -> IO b) -- ^ arg value parser (b is prior value)
         -> Lens' o b                   -- ^ lens onto opts for value store
         -> OptParse o
setvalm' = setValue ValOpt . ignoreFirst

-- setvalm_ --------------------------------------------------------------------

-- | parse a command-line string: does it mean 'True'?

parseb :: String -> Bool
parseb s = case lc s of
             "true"  -> True
             "yes"   -> True
             "1"     -> True
             "false" -> False
             "no"    -> False
             "0"     -> False
             _       -> error $    "failed to parse '" ++ s ++ "' as a Bool"

-- | version of setvalm for Bool values; parses true/yes/1 for true, false/no/0
--   for no defaults to True if no value passed

setvalm_ :: Lens' o (Maybe Bool) -> OptParse o
setvalm_  =  setvalm (\ m -> case m of
                               Nothing -> return True -- called with no
                                                      -- explicit value
                               Just m' -> return $ parseb m'
                     )

-- | logical inversion of setvalm_ (sets values to False, or opposite of
--   optional arg

setvalm__ :: Lens' o (Maybe Bool) -> OptParse o
setvalm__  =  setvalm (\ m -> case m of
                                Nothing -> return False -- called with no
                                                       -- explicit value
                                Just m' -> return . not $ parseb m'
                      )



-- setValue --------------------------------------------------------------------

-- | core setval fn for single-arg & no-arg options

setValue :: (NFData b)
         => ValSemantic                           -- ^ does this option take
                                                  --   an argument
         -> (String -> Maybe String -> b -> IO b) {- ^ arg value parser; String
                                                       is option name, Maybe
                                                       String is option cmdline
                                                       value
                                                       (Nothing for no value
                                                       passed); b is prior
                                                       stored value
                                                   -}
         -> Lens' o b                             -- ^ lens onto opts for value
                                                  --   store
         -> OptParse o

setValue req parser = setValue' f
  where f opt_name mb_optval largs old_v =
          let (optval, new_args, e) = maybeGetArg req mb_optval largs opt_name
           in case e of
                Just e_ -> return (Left e_, new_args)
                Nothing -> do
                  t <- try $ parser opt_name optval old_v >>= evaluate . force
                  case t of
                    Left (ex :: SomeException) ->
                      return (Left (err_badval optval ex opt_name), new_args)
                    Right v' -> return (Right v', new_args)

{- | depending on the value semantic, maybe use the initial optional value, else
     take none or one value from the list, returning the value to use (or
     not); a new list (the old one, drop n for some n), optionally an error
     string
 -}
maybeGetArg :: ValSemantic -- ^ whether we require one, maybe one, or no values
            -> Maybe a     -- ^ a value given by fiat (--foo=bar)
            -> [a]         -- ^ values to consume from (later cmdline args)
            -> String      -- ^ invoked option name, for errors
            -> (Maybe a, [a], Maybe String) {- ^ value to use, if any; new
                                                 (sub)list of values for later
                                                 consumption; maybe error string
                                             -}

maybeGetArg ValMandatory Nothing  []      opt_name = (Nothing, [],
                                                     Just $ err_reqval opt_name)
maybeGetArg ValMandatory Nothing  (h : t) _        = (Just h, t, Nothing)
maybeGetArg _            Nothing  largs   _        = (Nothing, largs, Nothing)
maybeGetArg ValNone      (Just _) largs   opt_name = (Nothing, largs,
                                                     Just $ err_noarg opt_name)
maybeGetArg _            (Just o) largs   _        = (Just o, largs, Nothing)

-- setValue' -------------------------------------------------------------------

-- | core value-setting routine for generating parsers; all others should be a
--   wrapper around this

setValue' :: (NFData b)
          => (String -> Maybe String -> [String] -> b
                     -> IO (Either String b, [String]))
                                                   {- ^ arg value parser; String
                                                        is option name, Maybe
                                                        String is option cmdline
                                                        value (after '=' or
                                                        similar)
                                                        (Nothing for no value
                                                        passed); [String] is any
                                                        remaining cmdline
                                                        strings; b is prior
                                                        stored value
                                                    -}
          -> Lens' o b                             -- ^ lens onto opts for value
                                                   --   store
          -> OptParse o

setValue' f l opt_name mb_optval state = do
  let old_v = state ^. (opts . l)
  t <- try $ f opt_name mb_optval (state ^. strs) old_v >>= evaluate . force
  case t of
    Left (e :: SomeException) ->
      parse_options (state & errs ~:~ show e)
    Right (v, new_strs) ->
      let new_state = case v of
                        Left e   -> state & errs ~:~ e
                        Right v' -> state & (opts . l) .~ v'
       in parse_options (new_state & strs .~ new_strs)

-- maybeGet2Args ---------------------------------------------------------------

{- | depending on the value semantic, maybe use the initial optional value, else
     take none, one or more values from the list, returning the value to use (or
     not); a new list (the old one, drop n for some n), optionally an error
     string
 -}
maybeGet2Args :: String       -- ^ delimiter to split key/value strings on
              -> Maybe String -- ^ a valuepair given by fiat (--foo=bar=>bax)
              -> [String]     -- ^ values to choose from (later cmdline args)
              -> String       -- ^ invoked option name, for errors
              -> (Either String (String,String), [String])
                              {- ^ values to use or error string; new (sub)list
                                   of values for later consumption
                               -}

maybeGet2Args _ Nothing [] opt_name = (Left $ err_reqkeyval opt_name, [])
maybeGet2Args d Nothing (h : t1) opt_name =
  case splitOn2 d h of
    (_, "")  -> -- delimiter not found, use next argument
                case t1 of
                  []     -> -- --opt key #END; we're missing a
                            -- value for the key
                            (Left $ err_reqkval h opt_name, [])
                  v : t2 -> (Right (h, v), t2)
    ("", h') -> -- delimiter is empty, use next argument
                case t1 of
                  []     -> -- --opt key #END; we're missing a
                            -- value for the key
                            (Left $ err_reqkval h' opt_name, [])
                  v : t2 -> (Right (h',v), t2)
    (k, v)   -> -- k, v found
                (Right (k,v), t1)

maybeGet2Args d (Just o) all_args opt_name =
  case splitOn2 d o of
    (_, "") -> -- delimiter not found, error
      (Left $ err_missing_kvdelim opt_name o d, all_args)
    ("", _) -> -- delimiter is empty, error
      (Left $ err_kv_nodelim opt_name o, all_args)
    (k, v)  -> -- k, v found
      (Right (k,v), all_args)

-- setvalAList' ----------------------------------------------------------------

{- | designed for alist-type options; expects either --opt=key=>value, or
     --opt key value, or --opt key=>value (where, in this example, the delimiter
     is '=>').  If delimiter is empty, then
     --opt=key=value & --opt key=value won't work; only --opt key value
     will be accepted in this case.  The key,value pair will be appended to
     the resultant alist.
 -}
setvalAList' :: (NFData k, NFData v)
             => String                           -- ^ delimiter
             -> (String -> [(k,v)] -> IO k)      -- ^ key parser
             -> (String -> k -> [(k,v)] -> IO v) -- ^ value parser
             -> Lens' o [(k,v)]                  -- ^ lens to storage in opts
             -> OptParse o

setvalAList' = setValue' ... f
  where f :: (NFData k, NFData v)
          => String                               -- ^ key-value delimiter
          -> (String -> [(k,v)] -> IO k)          -- ^ key parser
          -> (String -> k -> [(k,v)] -> IO v)     -- ^ value parser
          -> String                               -- ^ option name (for errs)
          -> Maybe String                         -- ^ option valstr
                                                  --   (if --opt=val)
          -> [String]                             -- ^ remaining cmdline args
          -> [(k,v)]                              -- ^ incoming k/v pairs
          -> IO (Either String [(k,v)], [String]) {- ^ either an error, or a
                                                       new kv alist; along
                                                       with remaining cmdline
                                                       args -}

        f delim parsek parsev opt_name mb_optval rem_args old_alist =
          let -- retrieve a key/value pair from the remaining arg list, if poss.
              (kv, new_args) = maybeGet2Args delim mb_optval rem_args opt_name
              -- check v for errors; if bad, run mkerr to generate a user error;
              -- else run g on the v
              check :: Either e s
                    -> (e -> String)
                    -> (s -> IO (Either String b, [String]))
                    -> IO (Either String b, [String])
              check v mkerr g = case v of
                                  Left err  -> return (Left (mkerr err), new_args)
                                  Right x -> g x
          in -- if kv is an error, just pass it through; else parse out the key,
             -- value and check those
             check kv id $ \ (k,v) -> do
               let e_badkey  e = err_badkey  k v e opt_name
                   e_badkval e = err_badkval k v e opt_name
               -- try to parse the key, in the context of the old alist (so we
               -- can check for repeated keys, etc.)
               k_ <- try $ parsek k old_alist >>= evaluate . force
               check k_ e_badkey $ \ k' -> do
                   -- try to parse the key, in the context of the old alist (so
                   -- we can check for repeated values, etc.)
                   v_ <- try $ parsev v k' old_alist >>= evaluate . force
                   check v_ e_badkval $ \ v' ->
                       return (Right (old_alist ++ [(k',v')]), new_args)

-- setvalAList -----------------------------------------------------------------

{- | designed for alist-type options; expects either --opt=key=value, or
     --opt key value, or --opt key=value.  The key,value pair will be appended
     to the resultant alist.
 -}
setvalAList :: (NFData v, NFData k) =>
               (String -> [(k, v)] -> IO k) ->
               (String -> k -> [(k, v)] -> IO v) ->
               Lens' o [(k, v)] ->
               OptParse o

setvalAList = setvalAList' "="

-- setvalt ---------------------------------------------------------------------

-- | special case of setValue for bool options
setvalt :: Lens' o Bool -> OptParse o
setvalt = setValue ValNone (\ _ _ _ -> return True)

-- | special case of setValue for negatively-setting bool options
setvalf :: Lens' o Bool -> OptParse o
setvalf = setValue ValNone (\ _ _ _ -> return False)

-- progName --------------------------------------------------------------------

-- | name of process in ps table or equiv
progName :: String
progName = unsafeDupablePerformIO getProgName

-- helpme ----------------------------------------------------------------------

-- | describe basics of program for help text

data HelpOpts = HelpOpts { prog_name :: String   -- ^ output program name
                         , arg_arity :: ArgArity -- ^ how many args accepted
                         , arg_type  :: String   -- ^ type of those args
                         }

instance Default HelpOpts where
  def = HelpOpts progName ArgOne ""

-- | construct --help option; uses progName, and no argstring
helpme' :: OptParse o
helpme' = helpme def

-- | construct --help option
helpme :: HelpOpts -> OptParse o
helpme helpopts _optname mb_optval state =
  let progname  = prog_name helpopts
      argstring = show_arity (arg_arity helpopts) (arg_type helpopts)
      -- | format up some help for a specific option
      make_help_for :: Option o -> String
      make_help_for o = intercalate "\n"
                                    ((:) ((++ ":") . intercalate "\n" .
                                            fmap (tr1 ' ' '|') . stripEnds .
                                            wrap 80 . unwords $
                                            optionNames o
                                         )
                                         (stripEnds $
                                            hjoin VTop [ mkBlock ["  "]
                                                       , wrap 78 (o ^. desc) ]
                                         )
                                    )
   in case mb_optval of
        Nothing -> let summ = mkHelpSummary progname argstring (state ^. cfg)
                    in parse_options (sethelp state "--help" summ)
        Just o  -> case filter (nameMatch o) (state ^. cfg) of
                     [opt]   -> case opt ^. desc of
                                  "" -> parse_options (state &
                                                  errs ~:~ err_nolonghelp o)
                                  _  -> parse_options (sethelp state
                                                          (   "--help"
                                                           ++ optionName opt)
                                                          (make_help_for opt))
                     []      -> parse_options (state & errs ~:~ err_nonopt o)
                     _ : _   -> error $ err_ambgopt o

-- helpopt ---------------------------------------------------------------------

-- | printable description for a single option (typically one line of the help
--   summary); each block is a column (so name, descn)
helpopt :: Int -> Maybe Int -> Maybe Int -> Int -> Option o -> [Block]
helpopt name_width type_width default_width desc_width o =
  let wrapw w  = wrap' def { wrap_width = w, wrap_prefix = "  " }
      my_wrapl = wrapw name_width
      my_wrapr = wrapw desc_width
      pipe :: String -> String
      pipe s = let (start, middle, end) = spanEnds (== ' ') s
                in start ++ tr1 ' ' '|' middle ++ end
      typeblock = maybe [] (\t -> [wrap t (o ^. typename)]) type_width
      dfltblock = maybe [] (\t -> [wrap t (o ^. dflt)]) default_width
   in concat [ [ mkBlock . fmap pipe . strings . my_wrapl .
                   unwords $ optionNames o ]
             , typeblock
             , dfltblock
             , [ my_wrapr (o ^. summary) ]
             ]

-- mkHelpSummary -----------------------

-- | format up printable help summary for a set of options
mkHelpSummary :: String -- ^ prog_name
              -> String -- ^ argument string (see helpme')
              -> [Option o]
              -> String
mkHelpSummary progname argstring options =
  let term_width = 78 -- leading "  "
      separator = "  "
      mytable = table' def { table_col_separator = separator }
      -- 16 + 62 == 78; leave two for the column separator
      -- prefix 0 : lest opts is empty
      name_width = min 16 . maximum $
                     (0 : fmap (length . unwords .  optionNames) options)
      type_width = min 8 . maximum $ (0 : fmap (length . view typename) options)
      dflt_width = min 8 . maximum $ (0 : fmap (length . view dflt) options)
      just n     = if n == 0 then Nothing else Just n
      widths     = catMaybes [ Just name_width, just type_width, just dflt_width ]
      -- width of descn part
      -- 2* for "  " separator
      width = term_width - sum widths - length separator * (length widths - 1)
      usage =    "usage: " ++ progname
              ++ (if not $ null options then " <option>*" else "")
              ++ (if not $ null argstring then ' ' : argstring else "")
      opts_table = mytable $ fmap (helpopt name_width
                                           (just type_width)
                                           (just dflt_width)
                                           width
                                  )
                                  options
   in usage ++ "\n\noptions:\n  " ++ intercalate "\n  " (strings opts_table)

-- parse_options ---------------------------------------------------------------

{- | parse a the next remaining strings (found in ParseState), and handle it
     per options-parsing config. (also found in ParseState); recursing to handle
     subsequent strings as required
 -}
parse_options :: ParseState o -> IO (ParseState o)
parse_options state =
  let -- | apply an option, possibly consuming values from the strs
      apply_opt :: ParseState o -- ^ initial state of parsing
                -> String       -- ^ option name as invoked, e.g., "foo" for
                                --   "--foo"
                -> Maybe String -- ^ option command-line string, if any
                -> IO (ParseState o)

      apply_opt state' oname mb_optval =
        case filter (nameMatch oname) (state' ^. cfg) of
          [] -> -- unrecognized option
            parse_options (state' & errs ~:~ err_nonopt oname)
          o : _  -> -- option with parsed value
            (o ^. parse) oname mb_optval state'

      -- | parse a single option (and recurse to parse_options)
      parse_opt :: ParseState o -> String -> IO (ParseState o)
      parse_opt state' opt =
        let (oname, mb_optval) = case span (/= '=') (dropWhile (== '-') opt) of
                                   (o, "") -> (o, Nothing)
                                   (o, m)  -> (o, Just $ tail m)
        in apply_opt state' oname mb_optval

      -- | special handling for single-char options to allow for clustering
      --   without confusing a cluster of -ox with the single option --ox
      parse_single_char_opt :: ParseState o  -- ^ initial state of parsing
                            -> String        -- ^ opt(s) to parse,
                                             --   e.g., "ox" for "-ox"
                            -> IO (ParseState o)

      parse_single_char_opt state' opt =
        let (options, value) = span (/= '=') (tail opt)
            (_valid, inv) = partition validShortOptName options
            state_e       = state' & errs ++= fmap err_invname inv
         in if null value
            then foldM (\s c -> apply_opt s [c] Nothing) state_e options
            else if 1 == length options
                 then parse_opt state_e opt
                 else parse_options $ state_e & errs ~:~ err_combine opt


  in case state ^. strs of
       -- no more strings to parse
       []                 -> return state

       -- '--' terminates parsing; all else are args
       "--" : as          -> return $ (state & strs .~ []) & args =++ reverse as

       -- '--foo'
       opt@('-':'-':_) :_ -> parse_opt (tl1 state) opt

       -- '-x'; including clustered '-abcd' and '-a=foo'
       opt@('-' : _) : _  -> parse_single_char_opt (tl1 state) opt

       -- plain argument
       s : _              -> parse_options (tl1 state & args ~:~ s)

-- ignoreFirst -----------------------------------------------------------------

-- | ignore first value

ignoreFirst :: (a -> b) -> z -> a -> b
ignoreFirst f _ = f

-- NFHandle --------------------------------------------------------------------

-- | Handle that is an instance of NFData.  We use a newtype here to avoid an
--   orphaned type.
-- instance NFData Handle where
newtype NFHandle = NFHandle Handle
  deriving Eq
instance Show NFHandle where
  show = show . unhandle
instance NFData NFHandle where

-- | extract handle
unhandle :: NFHandle -> Handle
unhandle (NFHandle h) = h

-- ... -------------------------------------------------------------------------

-- | like (.), but for three-valued functions
(...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(g ... f) a b c = g (f a b c)
