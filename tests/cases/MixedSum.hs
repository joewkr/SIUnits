{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((+))

test = a + b
  where
    a :: Double
    a = 1.0

    b :: SI M Double
    b = SI 1.0