{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((/))

test :: SI S Double
test = a / b
  where
    a :: SI (M * S) Double
    a = SI 1.0

    b :: SI (Kilo M) Double
    b = SI 1.0