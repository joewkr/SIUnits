{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CompileTest where

import Numeric.Units.SI
import Prelude hiding((+))


testCase1 = a + b
  where
    a :: SI (S ^ (P1%P2)) Double
    a = SI 1.0

    b :: SI (S ^ (P1%P2)) Double
    b = SI 1.0


testCase2 = a + b + c
  where
    a :: SI (S ^ (M1%P2)) Double
    a = SI 1.0

    b :: SI (S ^ (P1%M2)) Double
    b = SI 1.0

    c :: SI (I / S ^ (P1%P2)) Double
    c = SI 1.0


testCase3 = a + b
  where
    a :: SI (S ^ (P1%P2)) Double
    a = SI 1.0

    b :: SI (S ^ (M1%M2)) Double
    b = SI 1.0