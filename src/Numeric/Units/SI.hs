{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.Units.SI(
      module Numeric.Units.SI.Base
    , module Numeric.Units.SI.Derived
    , module Numeric.Units.SI.Numerals
    , SI(..)
    , (*), (/), (+), (-)
    , (**), (^^)
    , pi
    , exp, log
    , sin, cos, tan, asin, acos, atan
    , sinh, cosh, tanh, asinh, acosh, atanh
    , sqrt

    , (^), zero
    , p9, p8, p7, p6, p5, p4, p3, p2, p1
    , m9, m8, m7, m6, m5, m4, m3, m2, m1

    , sum, product ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Prelude as P

import Numeric.Units.SI.Base
import Numeric.Units.SI.Derived
import Numeric.Units.SI.Numerals

data SI (a :: Unit) b where
    SI :: !b -> SI a b deriving Generic

deriving instance P.Eq b => P.Eq (SI a b)
deriving instance P.Ord b => P.Ord (SI a b)
deriving instance P.Show b => P.Show (SI a b)

instance NFData b => NFData (SI a b)

instance P.Functor (SI a) where
    fmap f (SI val) = SI (f val)

infixl 7 *, /
infixl 6 +, -

(*) :: P.Num b => SI a1 b -> SI a2 b -> SI (Mult a1 a2) b
(*) (SI l) (SI r) = SI (l P.* r)

(/) :: P.Fractional b => SI a1 b -> SI a2 b -> SI (Div a1 a2) b
(/) (SI l) (SI r) = SI (l P./ r)

(+) :: P.Num b => SI a b -> SI a b -> SI a b
(+) (SI l) (SI r) = SI (l P.+ r)

(-) :: P.Num b => SI a b -> SI a b -> SI a b
(-) (SI l) (SI r) = SI (l P.- r)

-- Floating
pi :: P.Floating b => SI I b
pi = SI P.pi

exp, log :: P.Floating b => SI I b -> SI I b
sin, cos, tan, asin, acos, atan :: P.Floating b => SI I b -> SI I b
sinh, cosh, tanh, asinh, acosh, atanh :: P.Floating b => SI I b -> SI I b

exp = P.fmap P.exp
log = P.fmap P.log
sin = P.fmap P.sin
cos = P.fmap P.cos
asin = P.fmap P.asin
acos = P.fmap P.acos
atan = P.fmap P.atan
sinh = P.fmap P.sinh
cosh = P.fmap P.cosh
asinh = P.fmap P.asinh
acosh = P.fmap P.acosh
atanh = P.fmap P.atanh

tan = P.fmap P.tan
tanh = P.fmap P.tanh

sqrt :: P.Floating b => SI a b -> SI (a ^ (P1 % P2)) b
sqrt (SI b) = SI (P.sqrt b)

infixr 8 **, ^^, ^

(**) :: P.Floating b => SI I b -> b -> SI I b
(**) (SI b) e = SI (b P.** e)

(^^) :: (P.Fractional a, P.Integral b) => SI I a -> b -> SI I a
(^^) (SI b) e = SI (b P.^^ e)

data Power (p :: Boolean) (e :: Exp) where
    Power :: !P.Integer -> Power p e

p9 = Power 9; p9 :: Power 'BT P9
p8 = Power 8; p8 :: Power 'BT P8
p7 = Power 7; p7 :: Power 'BT P7
p6 = Power 6; p6 :: Power 'BT P6
p5 = Power 5; p5 :: Power 'BT P5
p4 = Power 4; p4 :: Power 'BT P4
p3 = Power 3; p3 :: Power 'BT P3
p2 = Power 2; p2 :: Power 'BT P2
p1 = Power 1; p1 :: Power 'BT P1
zero = Power 0; zero :: Power 'BT PZ
m1 = Power (-1); m1 :: Power 'BF P1
m2 = Power (-2); m2 :: Power 'BF P2
m3 = Power (-3); m3 :: Power 'BF P3
m4 = Power (-4); m4 :: Power 'BF P4
m5 = Power (-5); m5 :: Power 'BF P5
m6 = Power (-6); m6 :: Power 'BF P6
m7 = Power (-7); m7 :: Power 'BF P7
m8 = Power (-8); m8 :: Power 'BF P8
m9 = Power (-9); m9 :: Power 'BF P9

(^) :: P.Fractional b => SI a b -> Power p e -> SI (If p (a ^ e) (I / a ^ e)) b
(^) (SI b) (Power n) = SI (b P.^^ n)

sum :: forall t a b. (P.Num b, P.Foldable t) => t (SI a b) -> SI a b
sum = P.foldr (+) (SI 0 :: SI a b)

product :: forall t b. (P.Num b, P.Foldable t) => t (SI I b) -> SI I b
product = P.foldr (*) (SI 1 :: SI I b)
