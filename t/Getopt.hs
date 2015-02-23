#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

{-# LANGUAGE TemplateHaskell
 #-}

-- base --------------------------------

import Control.Exception  ( SomeException, try )
import Data.List          ( intercalate )
import System.IO          ( Handle, IOMode( ReadMode), openFile )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( (^.), makeLenses )

-- local imports -------------------------------------------

-- test-tap ----------------------------

import Test.TAP  ( explain, is, like, ok, test )

-- fluffy ------------------------------

import Fluffy.Language.TH.Type  ( readType )

-- this package --------------------------------------------

import Console.Getopt  ( Option
                       , getOptions, helpme', mkOpt, mkOptsB
                       , setvalOW, setvalm_, setval, setvalc, setvalc'
                       , setvals, setvals', setvalt, setvalf
                       , setvalAList'
                       )
--------------------------------------------------------------------------------

data Opts = Opts { _foo    :: Maybe String
                 , _bob    :: Int
                 , _bar    :: [Handle]
                 , _sett   :: Bool
                 , _quux   :: Bool
                 , _tett   :: Maybe Bool
                 , _uett   :: Bool
                 , _corn   :: String
                 , _list   :: [Int]
                 , _alist  :: [(Int, Handle)]
                 , _obool  :: Maybe Bool
                 , _obool2 :: Maybe Bool
                 }
  deriving (Show, Eq)

$( makeLenses ''Opts )

instance Default Opts where
  def = Opts Nothing 0 [] False False Nothing True "" [] [] Nothing Nothing

optCfg :: [Option Opts]
optCfg = [ mkOpt "fF" [ "foo", "fop", "fob"  ] (setval return foo)
                 "fooness" "foo desc" "" ""
         , mkOpt "b"  [ "bob"  ] (setvalOW (return . readType "Int") bob)
                 "bobness" (    "Bob Holness is not so well known as the first"
                             ++ " radio voice of James Bond" )
                 "" ""
         , mkOpt "a"  [ "bar"  ]
                 (setvals (`openFile` ReadMode) bar)
                 "barity"
                  (    "How many baas in a black sheep, that's all "
                    ++ "that you need to ask yourself" )
                  "" ""
         , mkOpt "qQu" [ "quux" ] (setvalt quux)
                "quuxoffalot" "come back here and I'll bite your ankles!" "" ""
         , mkOpt "s" [] (setvalt sett) "sett" "this is a setting option" "" ""
         ] 
         ++ mkOptsB "t" ["tett"] tett
                    "tett" "this is another setting option" "" "" ++
         [ mkOpt "x" [] (setvalf uett) 
                 "uett" "another setting option; def True" "" ""
         , mkOpt "c" [] (setvalOW return corn) "corn" "corn on the cob" "" ""
         , mkOpt [] [ "help" ] helpme'
                 "helpsicle"   "help me, Obi-Wan Kenobi, you're my only hope" 
                 "" ""
         , mkOpt "l" [] (setvals (return . readType "Int") list)
                 "list" "a list of integers" "" ""
         , mkOpt "C" [] (setvals' "," (return . readType "Int") list)
                 "clist" "a comma-separated list of integers" "" ""
         , mkOpt "A" [ "alist" ] (setvalAList' "=>"
                                              (const . return . readType "Int")
                                              (\ x _ _ -> openFile x ReadMode)
                                              alist )
                 "alist" "an alist of int to handle" "" ""
         , mkOpt "m" [ "optval" ] (setvalm_ obool)
                 "optval" "an optionally-valued option" "" ""
         , mkOpt "M" [ "optval2" ] (setvalm_ obool2)
                 "optval" "an optionally-valued option" "" ""
         , mkOpt "d" [] (setvalc' bob)
                 "decrement" "decrement an integer" "" ""
         , mkOpt "D" [] (setvalc bob)
                 "increment" "increment an integer" "" ""
         ]

-- with deliberate dups & a no-name option
optCfg2 :: [Option Opts]
optCfg2 = [ mkOpt "fF" [ "foo", "fop", "fob" ] (setval return foo)
                  "fooness" "foo desc" "" ""
          , mkOpt "bf" [ "bob", "foo" ]
                  (setvalOW (return . readType "Int") bob)
                  "bobness" "Bob Holness" "" ""
          , mkOpt ""  []
                  (setvalOW (\x -> sequence [openFile x ReadMode]) bar)
                  "barity" "how many?" "" ""
          ]

----------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

left :: Either a b -> a
left (Left a) = a
left (Right _) = error "left on a right"

main :: IO ()
main = do
  let err_args = [ "-f", "bart"
                 , "-F", "lisa"
                 , "--bar", "/etc/mot"
                 , "--bax", "--bay"
                 , "--bob", "seven"
                 , "--bob", "8"
                 , "--quux=True"
                 , "-sc=maize"
                 , "blobbie"
                 , "-l", "7.1"
                 , "-C", "3,three"
                 , "--alist=7", "/etc/motd"
                 , "-A=8=>/etc/passw"
                 , "-A=eight=>/etc/passwd"
                 , "--alist=nine=>/etc/passw"
                 , "--alist=9=/etc/passwd"
                 , "--optval=Maybe"
                 , "-t", "--no-tett"
                 , "--foo"
                 ]
      err1_exp = [    "failed to parse value 'lisa' for option 'F':\n" 
                   ++ "  option already set to '\"bart\"'"
                 ,  "failed to parse value '/etc/mot' for option 'bar':\n"
                   ++ "  /etc/mot: openFile: does not exist "
                   ++ "(No such file or directory)"
                 , "no such option: 'bax'"
                 , "no such option: 'bay'"
                 ,    "failed to parse value 'seven' for option 'bob':\n"
                   ++ "  failed to parse 'seven' as Int"
                 , "option 'quux' brooks no argument"
                 ,    "may not combine clustered single-char options with a "
                   ++ "value '-sc=maize'"
                 ,    "failed to parse value '7.1' for option 'l':\n"
                   ++ "  failed to parse '7.1' as Int"
                 ,    "failed to parse value '3,three' for option 'C':\n"
                   ++ "  failed to parse 'three' as Int"
                 , "option 'alist' assignment '7' is missing delimiter '=>'"
                 ,    "failed to parse value '/etc/passw' for option "
                   ++ "'A' key '8':\n  /etc/passw: openFile: does not exist"
                   ++ " (No such file or directory)"
                 ,    "failed to parse key 'eight' for option 'A'"
                   ++ " (value '/etc/passwd'):\n"
                   ++ "  failed to parse 'eight' as Int"
                 ,    "failed to parse key 'nine' for option 'alist'"
                   ++ " (value '/etc/passw'):\n"
                   ++ "  failed to parse 'nine' as Int"
                 ,    "option 'alist' assignment '9=/etc/passwd' is missing"
                   ++ " delimiter '=>'"
                 ,    "failed to parse value 'Maybe' for option 'optval':\n"
                   ++ "  failed to parse 'Maybe' as a Bool"
                 ,    "failed to enact option 'no-tett':\n" 
                   ++ "  option already set to 'True'"
                 , "option 'foo' requires a value"
                 ]

      help_exp  = [ ("-f|-F|--foo"  ,   "fooness")
                  , ("  --fop|--fob",   ""       )
                  , ("-b|--bob",        "bobness")
                  , ("-a|--bar",        "barity")
                  , ("-q|-Q|-u|--quux", "quuxoffalot")
                  , ("-s"     ,         "sett")
                  , ("-t|--tett",       "tett")
                  , ("--no-tett",       "logical inversion of -t|--tett")
                  , ("-x",              "uett")
                  , ("-c",              "corn")
                  , ("--help",          "helpsicle")
                  , ("-l",              "list")
                  , ("-C",              "clist")
                  , ("-A|--alist",      "alist")
                  , ("-m|--optval",     "optval")
                  , ("-M|--optval2",    "optval")
                  , ("-d"             , "decrement")
                  , ("-D"             , "increment")
                  ]
      pad n s = s ++ replicate (n - length s) ' '
      help    =    "usage: getopt-1 <option>*\n\noptions:\n  "
                ++ intercalate "\n  "
                     (fmap (\ (a,b) -> pad 15 a ++ "  " ++ pad 30 b) help_exp)
      dups_exp = [ "option has no name:"
                 , "  Option: \"\", [], barity, how many?, , "
                 , "option short name 'f' is used for multiple options:"
                 ,    "  Option: \"bf\", [\"bob\",\"foo\"], "
                   ++ "bobness, Bob Holness, , "
                 ,    "  Option: \"fF\", [\"foo\",\"fop\",\"fob\"], "
                   ++ "fooness, foo desc, , "
                 , "option long name 'foo' is used for multiple options:"
                 ,    "  Option: \"bf\", [\"bob\",\"foo\"], "
                   ++ "bobness, Bob Holness, , "
                 ,    "  Option: \"fF\", [\"foo\",\"fop\",\"fob\"], "
                   ++ "fooness, foo desc, , "
                 ]

  (args, opts, errs, helps) <- getOptions def optCfg err_args
  explain "args " args
  explain "opts " opts
  explain "errs " errs
  explain "helps" helps

  (args1, opts1, errs1, help1) <- getOptions def optCfg err_args

  (args2, opts2, errs2, help2) <- getOptions def optCfg
                                             [ "--help", "--help=b", "--help=bob" ]

  (args3, opts3, errs3, help3) <- getOptions def optCfg
                                             [ "one"
                                             , "--foo", "floo"
                                             , "--bob=7"
                                             , "two"
                                             , "-d", "-d", "-d", "-D"
                                             , "-l", "3"
                                             , "--quux"
                                             , "-a", "/etc/motd"
                                             , "--bar=/etc/group"
                                             , "-c=barley"
                                             , "-l", "2"
                                             , "-sx"
                                             , "-C", "5,7"
                                             , "-A", "7", "/etc/group"
                                             , "--alist", "8=>/etc/passwd"
                                             , "--alist=9=>/etc/passwd"
                                             , "-A=10=>/etc/group"
                                             , "-m=False"
                                             , "-M", "False"
                                             , "--no-tett=no"
                                             , "--"
                                             , "--three"
                                             , "--foo"
                                             ];

  dups <- try $ getOptions def optCfg2 []
            :: IO (Either SomeException ([String], Opts, [String], [String]))

  test [ like  args1  [ "blobbie", "/etc/motd" ]           "errs: args"
       , is    opts1  def { _foo = Just "bart", _bob = 8, _tett = Just True } 
                                                           "errs: no opts"
       , like  help1  []                                   "errs: no helps"
       , like  errs1  err1_exp                             "errs: errs"

       , is args2           []                                "help: no args"
       , is opts2           def                               "help: opt _bob=8"
       , is (length help2)  2                                 "help: length 2"
       , like (lines $ head help2)  (lines help)  "help: --help"
       , is (help2 !! 1)  (unwords [ "-b|--bob:\n  Bob Holness is"
                                   , "not so well known as the"
                                   , "first radio voice of James"
                                   , "Bond"
                                   ])
                                                              "help: --help=b"
       , is errs2           []                                "help: no errs"

       , is args3  [ "one", "two", "False", "--three", "--foo" ]    "opts: args"
       -- split out the bar,alist parts because they need to be 'show'n to be
       -- compared
       , is opts3  { _bar = [], _alist = [] }
            def { _foo = Just "floo", _bob = 5, _bar = [], _quux = True
                , _sett = True, _tett = Just True, _uett = False, _corn = "barley"
                , _list = [3,2,5,7], _obool = Just False, _obool2 = Just True
                }
           "opts"
       , is (show $ opts3 ^. bar)   "[{handle: /etc/motd},{handle: /etc/group}]"  
                                    "opts: bar /etc/motd"
       , is (show $ opts3 ^. alist)
            (intercalate "," [ "[(7,{handle: /etc/group})"
                             , "(8,{handle: /etc/passwd})"
                             , "(9,{handle: /etc/passwd})"
                             , "(10,{handle: /etc/group})]"
                             ])
            "opts: alist"
       , is help3  []                                      "opts: help"
       , is errs3  []                                      "opts: no errs"
       -- , explain "dups" dups
       , ok (isLeft dups) "left dups"
       , like (lines . show $ left dups) dups_exp "dups"
       ]
