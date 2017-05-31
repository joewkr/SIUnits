{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding(sqrt)

test :: SI (S / M ^ P2) Double
test = sqrt a
  where
    a :: SI (S ^ P2 / M ^ P4) Double
    a = SI 1.0