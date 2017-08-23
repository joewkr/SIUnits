{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((+))

type P18 = P9 .+. P9
test :: SI (M ^ P9 / S ^ P18 * Kg) Double
test = a + b
  where
    a :: SI (((M / S ^ P2) ^ P9) * Kg) Double
    a = SI 1.0

    b :: SI (M ^ P9 / S ^ P18 * Kg) Double
    b = SI 1.0