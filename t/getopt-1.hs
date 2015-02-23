{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
import Control.Lens  ( (^.), makeLenses )
import Data.Default  ( Default( def ) )
import Data.List     ( intercalate )

import Fluffy.Console.Getopt  ( ArgArity( ArgSome ), Option
                              , getopts, mkOpt, setval )

data Opts = Opts { _str :: Maybe String }
$( makeLenses ''Opts )

instance Default Opts where def = Opts Nothing

optCfg :: [Option Opts]
optCfg = [ mkOpt "s"  [ "str"  ] (setval return str)
                 "summary" "option description" "String" ""
         ]

main :: IO ()
main = do
  (args :: [Int], opts) <- getopts optCfg (ArgSome 1 3) "integer" (return . read)
  let s = maybe "" (++ " ") (opts ^. str)
  putStrLn $ s ++ unwords (fmap show args)
