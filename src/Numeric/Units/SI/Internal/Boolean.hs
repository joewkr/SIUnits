{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Internal.Boolean where

data Boolean where
    BT :: Boolean
    BF :: Boolean

type family If (c :: Boolean) (a :: k) (b :: k) :: k where
    If 'BT a b = a
    If 'BF a b = b

type family Not (b :: Boolean) :: Boolean where
    Not 'BT = 'BF
    Not 'BF = 'BT