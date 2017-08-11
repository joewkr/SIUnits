{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Numeric.Units.SI.Internal.Ternary where

import Data.Singletons.TH hiding (If, Min)
import Data.Singletons()

$(singletons [d|
  data Ternary where
    TBot :: Ternary
    TZ :: Ternary -> Ternary
    T1 :: Ternary -> Ternary
    TJ :: Ternary -> Ternary deriving Show
  |])

