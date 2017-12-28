{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((*))

test :: SI S Double
test = (u 2.0) * a
  where
    a :: SI S Double
    a = SI 1.0