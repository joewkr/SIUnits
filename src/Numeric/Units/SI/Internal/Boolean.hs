{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Internal.Boolean where

type family If (c :: Bool) (a :: k) (b :: k) :: k where
    If 'True a b = a
    If 'False a b = b

type family Not (b :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True