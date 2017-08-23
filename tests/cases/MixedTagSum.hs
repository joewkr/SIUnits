{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((+))

test :: SI (S) Double
test = a + b
  where
    a :: SI (Milli S) Double
    a = SI 1.0

    b :: SI (Micro S) Double
    b = SI 1.0