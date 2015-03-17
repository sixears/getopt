#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

{-# LANGUAGE TemplateHaskell #-}

-- base --------------------------------

import Control.Monad      ( forM_ )
import System.IO          ( IOMode( ReadMode), openFile )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( makeLenses )

-- Fluffy ------------------------------

import Fluffy.Language.TH.Type  ( readType )

-- this package --------------------------------------------

import Console.Getopt    ( ArgArity( ArgSome ), HelpOpts(..)
                         , NFHandle( NFHandle ), Option
                         , getopts, helpme, mkOpt, mkOptsB
                         , setvalOW, setval, setvalc, setvalc', setvals
                         , setvals', setvalt, setvalAList'
                         )
--------------------------------------------------------------------------------

data Opts = Opts { _string   :: Maybe String
                 , _int      :: Int
                 , _handles  :: [NFHandle]
                 , _bool     :: Bool
                 , _bool2    :: Maybe Bool
                 , _list     :: [Int]
                 , _alist    :: [(Int, NFHandle)]
                 , _mebbei   :: Maybe Int
                 }
  deriving (Show, Eq)

$( makeLenses ''Opts )

arity :: ArgArity
arity = ArgSome 1 3

instance Default Opts where
  def = Opts Nothing 0 [] False Nothing [] [] Nothing

optCfg :: [Option Opts]
optCfg = [ mkOpt "s"  [ "string"  ] (setval return string) "string" "String" 
                 "String" "\"\""
         , mkOpt "i"  [ "int"  ] (setvalOW (return . readType "Int") int)
                      "int" "Int" "Int" "0"
         , mkOpt "C"  [ "increment"  ] (setvalc int)
                      "increment" "Increment int" "Int" "0"
         , mkOpt "D"  [ "decrement"  ] (setvalc' int)
                      "decrement" "Decrement int" "Int" "0"
         , mkOpt "h"  [ "handle"  ]
                 (setvals ((fmap NFHandle) . (`openFile` ReadMode)) handles)
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
         , mkOpt "m" [] (setval (return . readType "Int") mebbei) 
                  "maybe int" "maybe int'" "?Maybe" ""
         , mkOpt "a" [ "alist" ] (setvalAList' "=>"
                                              (const . return . readType "Int")
                                              (\ x _ _ -> (fmap NFHandle) $ openFile x ReadMode)
                                              alist )
                 "alist int=>fn" "an alist of int to handle" 
                 "[(Int, Handle)]" "[]"
         , mkOpt ""  [ "help" ] 
                 (helpme def { arg_arity = arity, arg_type = "fn" })
                 "this help"  "Help" "" ""
         ]

----------------------------------------

main :: IO()
main = do
  (args, opts) <- getopts optCfg arity "int" 
                          (return . (readType "Int" :: String -> Int)) 
  forM_ [ "ARGS: " ++ show args, "OPTS:"  ++ show opts ] putStrLn
