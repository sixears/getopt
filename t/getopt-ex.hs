#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

{-# LANGUAGE TemplateHaskell #-}

-- base --------------------------------

import Control.Exception  ( SomeException, evaluate, try )
import Control.Monad      ( forM_ )
import Data.List          ( intercalate, sort )
import System.IO          ( Handle, IOMode( ReadMode), openFile )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( (&), (^.), (.~), (%~), Lens', makeLenses )

-- Fluffy ------------------------------

import Fluffy.Data.List         ( splitOn )
import Fluffy.Console.Getopt    ( ArgArity( ArgSome ), HelpOpts(..), Option
                                , getopts, helpme, mkOpt, mkOptsB, progName
                                , setvalOW, setval, setvalc, setvalc', setvals
                                , setvals', setvalt, setval', setvalAList'
                                )
import Fluffy.Language.TH.Type  ( readType )

--------------------------------------------------------------------------------

data Opts = Opts { _string   :: Maybe String
                 , _int      :: Int
                 , _handles  :: [Handle]
                 , _bool     :: Bool
                 , _bool2    :: Maybe Bool
                 , _corn     :: String
                 , _list     :: [Int]
                 , _alist    :: [(Int, Handle)]
                 }
  deriving (Show, Eq)

$( makeLenses ''Opts )

arity = ArgSome 1 3

instance Default Opts where
  def = Opts Nothing 0 [] False Nothing "" [] []

optCfg :: [Option Opts]
optCfg = [ mkOpt "s"  [ "string"  ] (setval return string) "string" "String" 
                 "String" "\"\""
         , mkOpt "i"  [ "int"  ] (setvalOW (return . (readType "Int")) int)
                      "int" "Int" "Int" "0"
         , mkOpt "C"  [ "increment"  ] (setvalc int)
                      "increment" "Increment int" "Int" "0"
         , mkOpt "D"  [ "decrement"  ] (setvalc' int)
                      "decrement" "Decrement int" "Int" "0"
         , mkOpt "h"  [ "handle"  ]
                 (setvals (\x -> openFile x ReadMode) handles)
                 "handle" "Handle" "[Handle]" "[]"
         , mkOpt "b" [ "bool" ] (setvalt bool)
                 (    "the quick brown fox jumped over the lazy dog, then fell "
                   ++ "down because the dog got up just as he was jumping, "
                   ++ "jumping, jumping, jumping high through the air like "
                   ++ "only a quick brown fox can jump"
                 )
                 "Boolean" "Bool" "False"
         ] 
         ++ mkOptsB "B" [ "Bool" ] bool2 "long-form +ve/-ve bool" "Boolean" 
                    "Bool"  "Nothing" ++
         [ mkOpt "l" [] (setvals (return . readType "Int") list)
                 "int list"  "[Int]" "[Int]" "[]"
         , mkOpt "c" [] (setvals' "," (return . readType "Int") list)
                 "comma-ilist" "a comma-separated list of integers" "[Int]" "[]"
         , mkOpt "a" [ "alist" ] (setvalAList' "=>"
                                              (const . return . readType "Int")
                                              (\ x _ _ -> openFile x ReadMode)
                                              alist )
                 "alist int=>fn" "an alist of int to handle" 
                 "[(Int, Handle)]" "[]"
         , mkOpt ""  [ "help" ] 
                 (helpme def { arg_arity = arity, arg_type = "fn" })
                 "this help"  "Help" "" ""
         ]

----------------------------------------

main = do
  (args, opts) <- getopts optCfg arity "filename" 
                          (return . (readType "Int" :: String -> Int)) 
  forM_ [ "ARGS: " ++ show args, "OPTS:"  ++ show opts ] putStrLn
