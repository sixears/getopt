{-# LANGUAGE TemplateHaskell #-}

-- base --------------------------------

import Control.Monad  ( forM_ )
import System.IO      ( Handle, IOMode( ReadMode ), openFile )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( (^.) )

-- fluffy ------------------------------

import Fluffy.Language.TH.Type  ( readType )

-- this package --------------------------------------------

import Console.Getopt    ( ArgArity( ArgSome ), mkOpt )
import Console.GetoptTH  ( CmdlineParseable(..), mkopts )

--------------------------------------------------------------------------------

--X data Opts = Opts { _string   :: Maybe String
--Y                  , _int      :: Int
--Y                  , _handles  :: [Handle]
--Y                  , _bool     :: Bool
--Y                  , _bool2    :: Maybe Bool
--Y                  , _corn     :: String
--Y                  , _list     :: [Int]
--Y                  , _alist    :: [(Int, Handle)]
--X                  }
--X   deriving (Show, Eq)

--X $( makeLenses ''Opts )

--X instance Default Opts where
--Y   def = Opts Nothing 0 [] False Nothing "" [] []
--X   def = Opts Nothing

-- optCfg :: [Option Opts]

newtype FileRO = FRO { getHandle :: Handle }

instance Show FileRO where
  show (FRO fn) = "FRO: " ++ show fn  

instance CmdlineParseable FileRO where
  enactOpt = (fmap FRO) . flip openFile ReadMode
  
-- use Optx to weed out any assumptions about Opt
$( mkopts "getoptsx" (ArgSome 1 3) "filename"
          [ "s|string::String#string summary"
          , "i|int::Int<4>#integer summary\ndefault 4"
          , "maybe-i|I::?Int#maybe integer summary\no default"
          , "mebbej|J::Maybe Int<Just 5>#maybe integer summary\ndefault just 5"
--          , "mebbes|S::?String#maybe string summary\nno default"
          , "incr|C::incr#increment summary\nincrement int longhelp"
          , "decr|D::decr<6>#decrement summary\ndecrement int longhelp"
          , "handle::filero</etc/motd>#read-only file\nauto-opened"
--          , "handle1::?filero#read-only file (no default)\nauto-opened"
          , "filero::*FileRO</etc/group>#IO handle (default /etc/group)"
--          , "mfilero::*?FileRO#IO handle (no default)\nauto-opened"
--          , "ip::TCP<127.0.0.1:80>#a TCP socket referred by ip address/hostname and port"

--          , "handles::filesro-<[/etc/passwd,/etc/group]>#read-only files\n"
--            ++ "no auto-open"

--          , "hurdles::[Handle]-<[/etc/passwd,/etc/group]></etc/motd>#"
--            ++ "files with start value != default"

--          , "humbles::[Handle]-<[/etc/passwd,/etc/group]>!myopen#"
--            ++ "files with custom parser"



--Y          , mkOpt "h"  [ "handle"  ]
--Y                  (setvals (\x -> openFile x ReadMode) handles)
--Y                  "handle" "Handle" "[Handle]" "[]"

--Y          , mkOpt "b" [ "bool" ] (setvalt bool)
--Y                  (    "the quick brown fox jumped over the lazy dog, then fell "
--Y                    ++ "down because the dog got up just as he was jumping, "
--Y                    ++ "jumping, jumping, jumping high through the air like "
--Y                    ++ "only a quick brown fox can jump"
--Y                  )
--Y                  "Boolean" "Bool" "False"
--Y          ]
--Y          ++ mkOptsB "B" [ "Bool" ] bool2 "long-form +ve/-ve bool" "Boolean"
--Y                     "Bool"  "Nothing" ++
--Y          [ mkOpt "l" [] (setvals (return . readType "Int") list)
--Y                  "int list"  "[Int]" "[Int]" "[]"
--Y          , mkOpt "c" [] (setvals' "," (return . readType "Int") list)
--Y                  "comma-ilist" "a comma-separated list of integers" "[Int]" "[]"
--Y          , mkOpt "a" [ "alist" ] (setvalAList' "=>"
--Y                                               (const . return . readType "Int")
--Y                                               (\ x _ _ -> openFile x ReadMode)
--Y                                               alist )
--Y                  "alist int=>fn" "an alist of int to handle"
--Y                  "[(Int, Handle)]" "[]"

--Z           example of parsing a custom type defined within this file
--A           example of strictly-positive integer (using custom munge)
--B           example of custom checker?  (else kill OptDescParse: _check
        ])

----------------------------------------

main :: IO ()
main = do
  (args, opts) <- getoptsx (return . (readType "Int" :: String -> Int))
  forM_ [ "ARGS: " ++ show args, "OPTS: "  ++ show opts ] putStrLn
  putStrLn $ "s     : "  ++ show (opts ^. s)
  putStrLn $ "i     : "  ++ show (opts ^. i)
  putStrLn $ "mebbei: "  ++ show (opts ^. maybe_i)
  putStrLn $ "mebbej: "  ++ show (opts ^. mebbej)
  putStrLn $ "incr  : "  ++ show (opts ^. incr)
  putStrLn $ "decr  : "  ++ show (opts ^. decr)
  putStrLn $ "handle: "  ++ show (opts ^. handle)
  putStrLn $ "filero: "  ++ show (getHandle $ opts ^. filero)
--  putStrLn $ "mfilero: " ++ show (opts ^. mfilero)
