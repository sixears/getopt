{-# LANGUAGE TemplateHaskell #-}

import Data.Default             ( Default( def ) )
import Console.Getopt           ( ArgArity( .. ) )
import Console.GetoptTH         ( mkopts )

import Console.GetoptTH         ( FileRO )
import Control.Monad            ( forM_ )
import Control.Lens             ( (^.) )
import Fluffy.Language.TH.Type  ( readType )

$( mkopts "getopts" (ArgSome 1 3) "integer"
          [ "bool|b::#just a bool"
          , "s|string::String#a string\nlong description"
          , "maybe-i|I>num::?Int#maybe integer summary\no default"
          , "handle::filero</etc/motd>#read-only file\nauto-opened"
          , "floats1::[,Float]<[1.0,2.0]><>#list of floats\nsplit on ','"
        ])

main :: IO ()
main = do
  (args, opts) <- getopts (return . (readType "Int" :: String -> Int))
  forM_ [ "ARGS: " ++ show args, "OPTS: "  ++ show opts ] putStrLn
  putStrLn $ "bool   : "  ++ show (opts ^. bool)
  putStrLn $ "s      : "  ++ show (opts ^. s)
  putStrLn $ "mebbei : "  ++ show (opts ^. num)
  putStrLn $ "handle : "  ++ show (opts ^. handle)
  putStrLn $ "floats1: "  ++ show (opts ^. floats1)
