{-# LANGUAGE TemplateHaskell #-}

{- |

Description : parsing a string into an OptDesc structure
Copyright   : (c) Martyn J. Pearce 2014, 2015
License     : BSD
Maintainer  : haskell@sixears.com

option descriptor access fns

-}

module Console.Getopt.OptDescParse
  ( _check )
where

-- base --------------------------------

import Text.Printf          ( printf )

-- template-haskell --------------------

import Language.Haskell.TH         ( ExpQ )
import Language.Haskell.TH.Syntax  ( newName
                                   , Exp( AppE, LamE, LitE, VarE )
                                   , Lit( IntegerL, StringL )
                                   , Pat( VarP )
                                   )

-- fluffy ------------------------------

import Fluffy.Data.List         ( splitBy2 )
import Fluffy.Language.TH       ( catchQ, checkBoolString, nameE )
import Fluffy.Language.TH.Type  ( readT )

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

readInt :: String -> Integer
readInt  = $(readT "Integer")

-- _check ----------------------------------------------------------------------

{- | verify an option value.

     First arg string which is used to create the check; if it contains a space,
     it will be treated as a leading operator and a trailing integer value;
     e.g., ">= 0" will check that the value is non-negative.  If it does not
     contain a space, it is treated as a function x -> Bool; the option value
     is passed in, and a False return will be considered as a verification
     failure.

     Second arg string is the name of the option as invoked; to use in the error
     message if the verification fails.
-}

-- > runQ [| \a -> if op a val then return a else fail $ printf "%s" (show a) |]
-- ===>
-- LamE [VarP a_6]
--      (CondE (AppE (AppE (VarE op_0) (VarE a_6)) (VarE val_0))
--             (AppE (VarE GHC.Base.return) (VarE a_6))
--             (InfixE (Just (VarE GHC.Base.fail))
--                     (VarE GHC.Base.$)
--                     (Just (AppE (AppE (VarE Text.Printf.printf)
--                                       (LitE (StringL "%s")))
--                                 (AppE (VarE GHC.Show.show) (VarE a_6))
--                           ))))

_check :: String -> String -> ExpQ
_check checkstr s =
  let a' = newName "a"
      (op_t, val_t) = splitBy2 (== ' ') checkstr
      (val, errmsg) =
        if null val_t
        then (0, printf "option %s val %%s failed check: '%s'" s op_t)
        else (readInt val_t,
              printf "option %s val %%s failed check: %s %s"
                     s op_t (show val)
             )
      errmsg_e      = LitE $ StringL errmsg
      -- f :: Exp( Bool ); e :: Exp( String )
      -- [| \a -> if f then return a else fail $ printf errmsg_e (show a) |]
--X--            validate a f e = (CondE f
--X--                                    (AppE (VarE 'return) (VarE a))
--X--                                    (InfixE (Just (VarE 'fail))
--X--                                            (VarE '($))
--X--                                            (Just (AppE (AppE (VarE 'printf) e)
--X--                                                        (AppE (VarE 'show)
--X--                                                              (VarE a))
--X--                                                  ))))

   in -- [| \a -> if op a val -- if a > 0 -- op a val
      --          then return a
      --          else fail $ printf "option %s val %s failed check: %s %s"
      --                             s (show a) op_t (show val) |]
      do a <- a'
         let f = -- Exp ( :: Bool )
                 if null val_t
                 then AppE (nameE op_t) (VarE a)
                 else AppE (AppE (nameE op_t) (VarE a))
                           (LitE (IntegerL (readInt val_t)))
--               c <- catchQ f -- Exp ( :: IO (Bool, String) )
         c <- catchQ (AppE (VarE 'return) f) -- Exp ( :: IO (Bool, String) )
--               c <- ((appE (varE 'return) f') >>= catchQ) -- Exp ( :: IO (Bool, String) )
--               m <- checkBoolString c errmsg_e -- Exp( Maybe String )
         m <- checkBoolString (return c) (return $ AppE (AppE (VarE 'printf) errmsg_e) (AppE (VarE 'show) (VarE a))) -- Exp( Maybe String )
         -- > runQ [| do (b,s) <- f;
         --           return $ if null s then "yes" else "no" |]
         -- ===>
         -- DoE [ BindS (TupP [ VarP b, VarP s ]) (VarE 'f)
         --     , NoBindS (InfixE (Just (VarE 'return))
         --                       (VarE '($))
         --                       (Just (CondE (AppE (VarE GHC.List.null)
         --                                          (VarE s))
         --                                    (LitE (StringL "yes"))
         --                                    (LitE (StringL "no")))))
         --     ]
--               return $ LamE [VarP a] (validate a f errmsg_e)
         return $ LamE [VarP a] m
         -- > runQ [| do (b,s) <- f;
         --           return $ if null s then "yes" else "no" |]
         -- ===>
         -- DoE [ BindS (TupP [ VarP b, VarP s ]) (VarE 'f)
         --     , NoBindS (InfixE (Just (VarE 'return))
         --                       (VarE '($))
         --                       (Just (CondE (AppE (VarE GHC.List.null)
         --                                          (VarE s))
         --                                    (LitE (StringL "yes"))
         --                                    (LitE (StringL "no")))))
         --     ]

