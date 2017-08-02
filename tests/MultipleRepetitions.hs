{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((+))

test :: SI (M * S ^ P2) Double
test = a + b
  where
    a :: SI (M * S ^ P2) Double
    a = SI 1.0

    b :: SI (S * M * S) Double
    b = SI 1.0