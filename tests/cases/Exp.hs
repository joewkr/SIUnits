{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((^))

test :: SI (S ^ P4) Double
test = a ^ p4
  where
    a :: SI S Double
    a = SI 1.0