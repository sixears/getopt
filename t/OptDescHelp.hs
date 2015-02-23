module OptDescHelp 
 -- export everything
where
  
-- this package --------------------------------------------

import Console.Getopt.OptDesc   ( OptDesc )

--------------------------------------------------------------------------------

-- double without default begets a Maybe Double type (value Nothing)
p3 :: OptDesc
p3 = read "z|y|xx>www::?Double#summary?" :: OptDesc
