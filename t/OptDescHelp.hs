module OptDescHelp 
 -- export everything
where
  
-- this package --------------------------------------------

import Console.Getopt.OptDesc   ( OptDesc )

--------------------------------------------------------------------------------

p0 :: OptDesc
p0 = read "z|y|xx>www::Double<1.1>" -- no summary

-- double without default begets a Maybe Double type (value Nothing)
p3 :: OptDesc
p3 = read "z|y|xx>www::?Double#summary?"
