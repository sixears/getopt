#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

-- base --------------------------------

import Data.List           ( isInfixOf, partition )
import System.Exit         ( ExitCode(..) )

-- containers --------------------------

import qualified Data.Map as Map

-- filepath ----------------------------

import System.FilePath.Posix  ( joinPath )

-- process -----------------------------

import System.Process  ( readProcessWithExitCode )

-- unix --------------------------------

import System.Posix.Directory  ( getWorkingDirectory )

-- local packages ------------------------------------------

-- test-tap ----------------------------

import Test.TAP  ( Test, diag, explain, is, like, ok, test )

-- Fluffy ------------------------------

import Fluffy.Data.List   ( splitOn2 )

--------------------------------------------------------------------------------

data Getoptsx = Getoptsx { _s      :: String
                         , _i      :: Int
                         , _incr   :: Int
                         , _decr   :: Int
                         , _handle :: HandleR
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
  readsPrec _ s = case span (/= ' ') (dropWhile (== ' ') s) of
                    ("{handle:", h)  ->
                      -- of course this will misbehave for any files with '}'
                      -- in the name
                      let (fn,rest) = span (/= '}') (tail h) -- head h == ' '
                       in [(HandleR fn, tail rest)]
                    _ -> error $ "no Handle parse of: '" ++ s ++ "'"

----------------------------------------

check_invocation :: (Read a, Eq a, Show a) =>
                    FilePath -> String -> [String]
                             -> Int -> [a] -> Maybe Getoptsx -> Map.Map String String
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
      items = Map.fromList $ fmap (splitOn2 ": ") rest
      nm = ((name ++ " ") ++)
      eexit :: Int -> ExitCode
      eexit 0 = ExitSuccess
      eexit e = ExitFailure e

  return [ is exit (eexit exp_exit)                               (nm "success")
         , explain "args" args
         , if null args
           then ok (null exp_args)                           (nm "empty args")
           else like (read args) exp_args                          (nm "args")
         , explain "opts" opts
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
  getWorkingDirectory >>= diag . ("cwd: " ++)
  let getopt_th    = joinPath [ "dist", "build", "getopt-th-hs", "getopt-th-hs" ]
      check        = check_invocation getopt_th

  let opt   = Getoptsx { _s = "", _i = 4, _incr = 0, _decr = 6
                       , _handle = HandleR "/etc/motd" }
      items = Map.fromList [ ("i", "4"), ("s", "\"\"")
                           , ("incr", "0"), ("decr", "6") ]

  (fmap concat . sequence)
    [ check "error invocation"  []    2 []  Nothing    Map.empty
            [ "! takes between 1 & 3 arguments (inclusive) (got none)" ]

    , check "simple invocation" ["7"] 0 [7 :: Int] (Just opt) items []

    , check "multiple arguments" [ "2", "3", "5" ] 0 [ 2, 3, 5 ] (Just opt)
            items []

    , check "args & opts" [ "2", "3", "--string", "bob", "--int", "8" ]
            0 [ 2, 3 ] (Just opt { _s = "bob", _i = 8 })
            (Map.fromList [ ("i", "8"), ("s", "\"bob\"") ] `Map.union` items) []

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
    ]
    >>= test