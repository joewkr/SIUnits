{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((*))

test :: SI (M * S) Double
test = a * b
  where
    a :: SI S Double
    a = SI 1.0

    b :: SI M Double
    b = SI 1.0