#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

-- base --------------------------------

import Data.List    ( intercalate, isInfixOf, partition )
import System.Exit  ( ExitCode(..) )

-- containers --------------------------

import qualified Data.Map as Map

-- filepath ----------------------------

import System.FilePath.Posix  ( joinPath )

-- lens --------------------------------

import Control.Lens  ( over, _1 )

-- process -----------------------------

import System.Process  ( readProcessWithExitCode )

-- local packages ------------------------------------------

-- test-tap ----------------------------

import Test.TAP  ( Test, is, like, ok, test )

-- Fluffy ------------------------------

import Fluffy.Data.List    ( splitOn2 )
import Fluffy.Data.String  ( stripEnd )

--------------------------------------------------------------------------------

readHandle :: (String -> a) -> String -> [(a, String)]
readHandle ctor s = case span (/= ' ') (dropWhile (== ' ') s) of
                      ("{handle:", h)  ->
                        -- of course this will misbehave for any files with '}'
                        -- in the name
                        let (fn,rest) = span (/= '}') (tail h) -- head h == ' '
                         in [(ctor fn, tail rest)]
                      _ -> error $ "no Handle parse of: '" ++ s ++ "'"

newtype FileRO = FRO { _getHandle :: String }
  deriving (Show, Eq)

instance Read FileRO where
  readsPrec _ s = readHandle FRO (drop 5 (dropWhile (== ' ') s))

data Getoptsx = Getoptsx { _s       :: String
                         , _i       :: Int
                         , _maybe_i :: Maybe Int
                         , _mebbej  :: Maybe Int
                         , _incr    :: Int
                         , _decr    :: Int
                         , _handle  :: HandleR
                         , _filero  :: FileRO
                         , _mfilero :: Maybe FileRO
--                         , _floats1 :: [Float]
                         , _floats2 :: [Float]
--                         , _ints1   :: [Int]
                         , _ints2   :: [Int]
                         }
  deriving (Eq, Show, Read)

----------------------------------------

-- | a bit like a Handle with a Read & Eq semantic; read "reads" the show
--   of a real Handle
newtype HandleR = HandleR String
  deriving Eq

instance Show HandleR where
  show (HandleR s) = "{handle: " ++ s ++ "}"

instance Read HandleR where
  readsPrec _ s = readHandle HandleR (dropWhile (/= ' ') (dropWhile (== ' ') s))

----------------------------------------

check_invocation :: (Read a, Eq a, Show a) =>
                    FilePath -> String -> [String]
                             -> Int -> [a] -> Maybe Getoptsx
                             -> Map.Map String String
                             -> [String]
                             -> IO [Test]

-- | run a cmd, check that it's successful and the outputs are as expected

check_invocation exec name iargs exp_exit exp_args exp_opt exp_items exp_err = do
  (exit, out, err) <- readProcessWithExitCode exec iargs ""
  let (outs, spare) = partition (": " `isInfixOf`) (lines out)
      -- ensure we don't have a pattern failure by whacking "" on the end
      -- if necessary
      (arstr : opt_str : rest) = outs ++ replicate (2 - length outs) ""
      (_argh, args) = splitOn2 ": " arstr
      (_opth, opts) = splitOn2 ": " opt_str
      items = (Map.fromList . fmap (over _1 stripEnd . splitOn2 ": ")) rest
      nm = ((name ++ " ") ++)
      eexit :: Int -> ExitCode
      eexit 0 = ExitSuccess
      eexit e = ExitFailure e

  return [ is exit (eexit exp_exit)                               (nm "success")
         -- , explain "args" args
         , if null args
           then ok (null exp_args)                           (nm "empty args")
           else like (read args) exp_args                          (nm "args")
         -- , explain "opts" opts
         , maybe (ok (null opts)                               (nm "no opts"))
                 (\o -> is (read opts) o                          (nm "opts"))
                 exp_opt
         , like spare []                                  (nm "unparsed stdout")
         , like (lines err) exp_err                                (nm "stderr")
         , is items exp_items                                       (nm "items")
         ]

----------------------------------------

main :: IO ()
main = do
  let getopt_th    = joinPath [ "dist", "build", "getopt-th-hs", "getopt-th-hs" ]
      check        = check_invocation getopt_th
      pad n s      = s ++ replicate (n - length s) ' '

  let opt   = Getoptsx { _s = ""
                       , _i = 4
                       , _incr = 0
                       , _decr = 6
                       , _maybe_i = Nothing
                       , _mebbej  = Just 5
                       , _handle  = HandleR "/etc/motd"
                       , _filero  = FRO "/etc/group"
                       , _mfilero = Nothing
--                       , _floats1 = []
                       , _floats2 = [9.8,7.6]
--                       , _ints1 = [2,3,5,7,11,13]
                       , _ints2 = [1,1,2,3]
                       }
      items = Map.fromList [ ("i", "4"), ("s", "\"\"")
                           , ("mebbei", "Nothing")
                           , ("mebbej", "Just 5")
                           , ("incr", "0"), ("decr", "6")
                           , ("handle", "FRO: {handle: /etc/motd}")
                           , ("filero", "{handle: /etc/group}")
                           , ("mfilero","Nothing")
--                           , ("floats1", "[]")
                           , ("floats2", "[9.8,7.6]")
--                           , ("ints1", "[2,3,5,7,11,13]")
                           , ("ints2", "[1,1,2,3]")
                           ]
  
  (exit, out, err) <- readProcessWithExitCode getopt_th ["--help"] ""

  (fmap concat . sequence)
    [ check "error invocation"  []    2 []  Nothing    Map.empty
            [ "! getopt-th-hs: takes between 1 & 3 arguments (inclusive) (got none)" ]

    , check "simple invocation" ["7"] 0 [7 :: Int] (Just opt) items []

    , check "multiple arguments" [ "2", "3", "5" ] 0 [ 2, 3, 5 ] (Just opt)
            items []

    , check "args & opts" [ "2", "3", "--string", "bob", "--int", "8" ]
            0 [ 2, 3 ] (Just opt { _s = "bob", _i = 8 })
            (Map.fromList [ ("i", "8"), ("s", "\"bob\"") ] `Map.union` items) []

    , check "ints2" [ "3", "--ints2", "8", "--ints2", "7", "--ints2", "6" ]
            0 [ 3 ] (Just opt { _ints2 = [5,8,8,7,6] })
            (Map.fromList [ ("ints2", "[5,8,8,7,6]") ] `Map.union` items) []

    , check "incr" [ "2", "3", "--incr" ]
            0 [ 2, 3 ] (Just opt { _incr = 1 })
            (Map.fromList [ ("incr", "1") ] `Map.union` items) []

    , check "multiple incr" [ "2", "3", "--string", "jim", "--int", "13"
                            , "--incr", "--incr", "5", "--incr" ]
            0 [ 2, 3, 5 ]
            (Just opt { _s = "jim", _i = 13, _incr = 3 })
            (Map.fromList [ ("i", "13") , ("s", "\"jim\"") , ("incr", "3") ]
             `Map.union` items)
            []

    , check "mebbe (1)" [ "2", "3", "--maybe-i", "14", "--mebbej", "Nothing" ]
            0 [ 2, 3 ]
            (Just opt { _maybe_i = Just 14, _mebbej = Nothing })
            (Map.fromList [ ("mebbei", "Just 14") , ("mebbej", "Nothing") ]
             `Map.union` items)
            []

    , check "mebbe (2)" [ "2", "3", "--mebbej", "Just 2" ]
            0 [ 2, 3 ]
            (Just opt { _mebbej = Just 2 })
            (Map.fromList [ ("mebbej", "Just 2") ] `Map.union` items)
            []

    , check "handles" [ "2", "3"
                      , "--handle", "/etc/passwd"
                      , "--filero", "/etc/ld.so.conf" 
                      , "--mfilero", "/etc/hostname" 
                      ]
            0 [ 2, 3 ]
            (Just opt { _handle = HandleR     "/etc/passwd"
                      , _filero = FRO         "/etc/ld.so.conf"
                      , _mfilero = Just $ FRO "/etc/hostname"
                      })
            (            Map.fromList [ ("filero", "{handle: /etc/ld.so.conf}")
                                      , ("mfilero","Just {handle: /etc/hostname}")
                                      , ("handle", "FRO: {handle: /etc/passwd}")
                                      ]
             `Map.union` items)
            []

    , check "nofile" [ "2", "3" , "--mfilero", "/etc/nostname" ]
            3 [] Nothing Map.empty
            [    "! getopt-th-hs: /etc/nostname: openFile: does not exist " 
              ++ "(No such file or directory)" ]

    , return [ is exit (ExitFailure 2) "--help exit is 2" 
             , like (lines out) 
               ([ "usage: getopt-th-hs <option>* <filename>{1,3}"
                , "", "options:" ] ++ 
               fmap (\ss -> "  " ++ intercalate "  " (zipWith pad [12,9,10,42] ss))
                    [ [ "name"        , "type"  , "default", "summary"         ]
                    , [ "-s|--string" , "String", "\"\""   , "string summary"  ]
                    , [ "-i|--int"    , "Int"   , "4"      , "integer summary" ]
                    , [ "-I|--maybe-i", "Int"   , ""       , 
                                                       "maybe integer summary" ]
                    , [ "-J|--mebbej" , "Maybe Int", 
                                                  "Just 5" , 
                                                       "maybe integer summary" ]
                    , [ "-C|--incr"   , "incr"  , "0"      , 
                                                           "increment summary" ]
                    , [ "-D|--decr"   , "decr"  , "6"      , 
                                                           "decrement summary" ]
                    , [ "--handle"    , "filero", "/etc/motd","read-only file" ]
                    , [ "--filero"    , "ROFile", "/etc/group", 
                                              "IO handle (default /etc/group)" ]
                    , [ "--mfilero"   , "ROFile", "", "IO handle (no default)" ]

                    , ["--floats2"    , "[Float]","[9.8,7.6]","list of floats" ]
                    , [ "--ints2"     , "[Int]",  "[1,1,2,3]", "list of ints"  ]
                    , [ "--help"      , "", "", 
                                  "this help; use --help=<opt> for detail (no" ]
                    ]
               ++ [    "                                         " 
                    ++ "leading hyphens on <opt>)               " ]
               )
               "--help stdout"
             , like (lines err) []   "--help stderr"
             ]
    ]
    >>= test
