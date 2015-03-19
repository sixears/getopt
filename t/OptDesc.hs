#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

{-# LANGUAGE FlexibleInstances
           , NoMonomorphismRestriction
           , OverlappingInstances
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeSynonymInstances
  #-}

-- base --------------------------------

import Control.Exception    ( SomeException, evaluate, try )

-- lens --------------------------------

import Control.Lens  ( (^.) )

-- template-haskell --------------------

import Language.Haskell.TH  ( Exp, ExpQ, runQ )

-- test-tap ----------------------------

import Test.TAP  ( explain, is, test )

-- fluffy ------------------------------

import Fluffy.Language.TH.Type  ( readType )
import Fluffy.Text.PCRE         ( substg )

-- this package --------------------------------------------

import Console.Getopt.OptDesc       ( descn, lensname, names, summary
                                           , pprintQ, typename )
import Console.Getopt.OptDescParse  ( _check )
import Console.Getopt.OptDescT      ( OptDesc(..) )
import Console.Getopt.OTypes        ( typeDefault )

import OptDescHelp

--------------------------------------------------------------------------------

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right _) = error "fromLeft on a Right"

ne7 :: Int -> Bool
ne7 = (/= 7)

nex :: String -> Bool
nex = (/= "x")

class QShow e where
  qshow :: e -> String

instance QShow Exp where
  qshow = show

instance QShow ExpQ where
  qshow = pprintQ

showQ :: QShow e => e -> String
showQ expr = let s = qshow expr
             in ("GHC.(?:Types|Base).(\\S*)" `substg` "$1") s

main :: IO ()
main = do
  c1 <- $( _check "> 0" "opt1" ) (1 :: Int)
  c2 <- $( _check "> 0" "opt1" ) (0 :: Int)
  c3 <- $( _check "ne7" "opt2" ) (1 :: Int)
  c4 <- $( _check "ne7" "opt2" ) (7 :: Int)
  c5 <- $( _check "nex" "opt3" ) "y"
  c6 <- $( _check "nex" "opt3" ) "x"

  -- construct an OptDesc; test stringification
  -- construct an OptDesc; test stringification - maybe (no default)
  -- test parsing of string to OptDesc
  --   - types; X -> Maybe X if no default provided
  --            [X]
  --            {X,Y}
  -- construct an Option from an OptDesc; test expected values
  -- use a better munger than _ -> 9.7; test the munger
  -- what's "check" for?
-- XXX test dflt using (...) and {...}

  -- p0 --------------------------------

  let p0 = read "z|y|xx>www::Double<1.1>" :: OptDesc -- no summary
      show_p0 = is (show p0) "z|y|xx>www::Double<1.1>"                 "show p0"

  -- p1 --------------------------------

  let p1 = OptDesc { _names    = [ "a", "b", "cd", "ef" ]
                   , _lensname = "ghi"
                   , _typename = "Float"
                   , _dflt     = [| 7.0 |]
                   , _strt     = [| 5.5 |]
                   , _summary  = "summ"
                   , _descn    = ""
-- pre-munge (String -> X)?  post-munge (X -> X)?  Not worry for now?
-- pre- & post-munge could be part of the parse fn
                   , _munge    = \_ -> [| 9.7 |]
                   }

      names_p1     = is (p1 ^. names) [ "a", "b", "cd", "ef" ]        "names p1"
      lensname_p1  = is (p1 ^. lensname) "ghi"                     "lensname p1"
      type_p1      = is (p1 ^. typename) "Float"                   "typename p1"

  d1_exp <- runQ [| 7.0 |]
  s1_exp <- runQ [| 5.5 |]

  d1 <- runQ $ _dflt p1
  s1 <- runQ $ _strt p1

  let dflt_p1     = is (show d1) (showQ d1_exp)                        "dflt p1"
      strt_p1     = is (show s1) (showQ s1_exp)                        "strt p1"

  let summ_p1     = is (p1 ^. summary) "summ"                          "summ p1"
  let dscn_p1     = is (p1 ^. descn) ""                                "dscn p1"

      -- show of an OptDesc
  let show_p1   = is (show p1)
                      "a|b|cd|ef>ghi::Float<7 / 1><11 / 2>#summ"       "show p1"
      -- single long name
      p1_0      = p1 { _names = [ "ef" ] }
      show_p1_0 = is (show p1_0)
                     "ef>ghi::Float<7 / 1><11 / 2>#summ"             "show p1_0"
      -- single short name
      p1_1      = p1 { _names = [ "f" ] }
      show_p1_1 = is (show p1_1)
                     "f>ghi::Float<7 / 1><11 / 2>#summ"              "show p1_1"

      -- first name matches lensname
      p1_2      = p1 { _names = [ "ghi", "f" ] }
      show_p1_2 = is (show p1_2)
                     "ghi|f::Float<7 / 1><11 / 2>#summ"              "show p1_2"

      -- first name matches lensname
      p1_3      = p1 { _names = [ "ghi", "f" ], _descn = "foo\nbar" }
      show_p1_3 = is (show p1_3)
                     "ghi|f::Float<7 / 1><11 / 2>#summ\nfoo\nbar"    "show p1_3"

  -- p2 --------------------------------

  -- double with default, using a qualified (full) name
  let p2 = read "z|y|xx>www::Double<1.1>#summary?\nbaz\nquux" :: OptDesc

  d2 <- runQ $ _dflt p2
  d2_exp <- runQ [| (readType "Double" :: String -> Double) "1.1" |]

  let show_p2 =
        is (show p2) "z|y|xx>www::Double<1.1>#summary?\nbaz\nquux"     "show p2"
      dflt_p2 = is (showQ d2) (showQ d2_exp)                           "dflt p2"
      dscn_p2 = is (p2 ^. descn) "baz\nquux"                           "descn p2"

  -- p3 --------------------------------

  -- default to Nothing
  -- defined in OptDescHelp

  -- p3 = read "z|y|xx>www::Data.Maybe.Maybe Double#summary?" :: OptDesc

  let show_p3 =
        is (show p3) "z|y|xx>www::?Double#summary?"                    "show p3"
  d3 <- runQ $ _dflt p3
  d3_exp <- runQ [| Nothing |]
  let dflt_p3 = is d3 d3_exp                                           "dflt p3"

  -- p4 --------------------------------

  let p4 = read "s|string::String<bob>#mystring\ndescn" :: OptDesc
      names_p4     = is (p4 ^. names) [ "s", "string" ]               "names p4"
      lensname_p4  = is (p4 ^. lensname) "s"                       "lensname p4"
      type_p4      = is (p4 ^. typename) "String"                  "typename p4"

  d4_exp <- runQ [| (readType "String" :: String -> String) "bob" |]
  s4_exp <- runQ [| Nothing |]

  d4 <- runQ $ _dflt p4
  s4 <- runQ $ _strt p4

  let dflt_p4     = is (showQ d4) (showQ d4_exp)                       "dflt p4"
      strt_p4     = is (showQ s4) (showQ s4_exp)                       "strt p4"

  let summ_p4     = is (p4 ^. summary) "mystring"                      "summ p4"
  let dscn_p4     = is (p4 ^. descn) "descn"                           "dscn p4"

  -- p5 --------------------------------

  let p5 = read "int|i::Int<0><13>#myint\nlongdesc" :: OptDesc
      names_p5    = is (p5 ^. names) [ "int", "i" ]                   "names p5"
      lensname_p5 = is (p5 ^. lensname) "int"                      "lensname p5"
      type_p5     = is (p5 ^. typename) "Int"                      "typename p5"

  d5 <- runQ $ _dflt p5
  d5_exp <- runQ [| (readType "Int" :: String -> Int) "0" |]

  let dflt_p5     = is (showQ d5) (showQ d5_exp)                       "dflt p5"

  s5 <- runQ $ _strt p5
  -- weird forms of strings to account for pprint strangeness in ghc7.8.3 /
  -- template-haskell-2.9.0.0; we should really handle this in showQ, (by
  -- changing ListE [LitE (CharL 'I'),...] to LitE (String L "I...")
  -- s5_exp <- runQ [| readType ['I','n','t'] ['1','3'] :: Int |]
  s5_exp <- runQ [| readType "Int" "13" :: Int |]

  let strt_p5     = is (showQ s5) (showQ s5_exp)                       "strt p5"

  let summ_p5     = is (p5 ^. summary) "myint"                         "summ p5"
  let dscn_p5     = is (p5 ^. descn) "longdesc"                        "dscn p5"

  -- p7 --------------------------------

  p7 :: Either SomeException OptDesc
      <- try $ evaluate (read "z|y|xx>www::Double<1.1>!" :: OptDesc)

  let err_p7 =
        is (show (fromLeft p7))
           "failed to parse option 'z|y|xx>www::Double<1.1>!' at '!'"
                                                                        "err p7"

  -- p8 --------------------------------

  p8 :: Either SomeException OptDesc
      <- try $ evaluate (read "Z|y|xx::Double<1.1>" :: OptDesc)

  let err_p8 =
        is (show (fromLeft p8))
           "lens 'Z' may not begin with an upper-case letter"
                                                                        "err p8"

  -- p9 --------------------------------

  let p9 = read "incr|I::incr<6>#increment" :: OptDesc
      names_p9    = is (p9 ^. names) [ "incr", "I" ]                  "names p9"
      lensname_p9 = is (p9 ^. lensname) "incr"                     "lensname p9"
      type_p9     = is (p9 ^. typename) "incr"                     "typename p9"

  d9 <- runQ $ _dflt p9
  d9_exp <- runQ [| (readType "Int" :: String -> Int) "6" |]

  explain "p9" p9
  let dflt_p9     = is (showQ d9) (showQ d9_exp)                       "dflt p9"

  s9 <- runQ $ _strt p9
  s9_exp <- runQ [| (readType "Int" :: String -> Int) "6" |]

  let strt_p9     = is (showQ s9) (showQ s9_exp)                       "strt p9"

  let summ_p9     = is (p9 ^. summary) "increment"                     "summ p9"
  let dscn_p9     = is (p9 ^. descn) ""                                "dscn p9"

  -- test ----------------------------------------------------------------------

  let typeD = maybe "-NONE-" showQ . typeDefault

  test [ is (typeD "String") (pprintQ [| "" |])             "typeDefault String"
       , is (typeD "Int") (pprintQ [| 0 |])                    "typeDefault Int"
       , is (typeD "Float") "-NONE-"                         "typeDefault Float"
       , is (typeD "?String") (pprintQ [| Nothing |])
                                                           "typeDefault ?String"
       , is (typeD "Maybe Int") "-NONE-"
                                                         "typeDefault Maybe Int"
       , is (typeD "?[[Int]]") (pprintQ [| Nothing |])
                                                          "typeDefault ?[[Int]]"
       , is (typeD "[Int]") (pprintQ [| [] |])               "typeDefault [Int]"
       , is (typeD "[[Int]]") (pprintQ [| [] |])           "typeDefault [[Int]]"

       , show_p0
       , show_p1

       , names_p1
       , lensname_p1
       , type_p1
       , dflt_p1
       , strt_p1
       , summ_p1
       , dscn_p1

       , show_p1_0
       , show_p1_1
       , show_p1_2
       , show_p1_3

       , show_p2
       , dflt_p2

       -- , diag $ show p7 -- get a meaningful parse error
       , err_p7

       -- get a meaningful parse error for upper-case first letter on first arg
       -- name
       , err_p8

       , show_p3
       , dflt_p3
       , dscn_p2
       -- , explain "p3" p3

       , names_p4
       , lensname_p4
       , type_p4
       , dflt_p4
       , strt_p4
       , summ_p4
       , dscn_p4

       , names_p5
       , lensname_p5
       , type_p5
       , dflt_p5
       , strt_p5
       , summ_p5
       , dscn_p5

       , names_p9
       , lensname_p9
       , type_p9
       , dflt_p9
       , strt_p9
       , summ_p9
       , dscn_p9

       -------------------------------------------------------------------------

       , is c1 Nothing                                          "check > 0 pass"
       , is c2 (Just "option opt1 val 0 failed check: > 0")     "check > 0 fail"
       , is c3 Nothing                                          "check ne7 pass"
       , is c4 (Just "option opt2 val 7 failed check: 'ne7'")   "check ne7 pass"
       , is c5 Nothing                                          "check nex pass"
       , is c6 (Just "option opt3 val \"x\" failed check: 'nex'")
                                                                "check nex fail"
       ]
