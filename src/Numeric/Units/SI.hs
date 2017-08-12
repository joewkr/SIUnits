{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.Units.SI(
      module Numeric.Units.SI.Base
    , module Numeric.Units.SI.Derived
    , module Numeric.Units.SI.Internal.Numerals
    , module Numeric.Units.SI.Tags.Multiply.Prefixes
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


import Data.Singletons
import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Prelude as P

import Numeric.Units.SI.Base
import Numeric.Units.SI.Derived
import Numeric.Units.SI.Internal.Numerals
import Numeric.Units.SI.Tags.Multiply.Prefixes

newtype SI (a :: Unit) b where
    SI :: {unSI :: b} -> SI a b deriving Generic

deriving instance P.Eq b => P.Eq (SI a b)
deriving instance P.Ord b => P.Ord (SI a b)
deriving instance P.Show b => P.Show (SI a b)

instance NFData b => NFData (SI a b)

instance P.Functor (SI a) where
    fmap f (SI val) = SI (f val)

infixl 7 *, /
infixl 6 +, -


data ModifyingTagType where
    None :: ModifyingTagType
    Submultiple :: ModifyingTagType
    Multiple :: ModifyingTagType

type family HasModifyingTag (a :: Unit) :: ModifyingTagType where
    HasModifyingTag u = If (HasMultTag u)
        (If (HasReducingMultTag u) 'Submultiple 'Multiple) 'None

type family SelectMode (a :: ModifyingTagType) (b :: ModifyingTagType) :: ModifyingTagType where
    SelectMode 'Submultiple b = 'Submultiple
    SelectMode a 'Submultiple = 'Submultiple
    SelectMode 'Multiple 'None = 'Multiple
    SelectMode 'None 'Multiple = 'Multiple
    SelectMode 'None 'None = 'None

type family TresM (r :: ModifyingTagType) (a1 :: Unit) (a2 :: Unit) :: Unit where
    TresM 'None a1 a2 = (a1 * a2)
    TresM r a1 a2 = DropMultTag (a1 * a2)

type family TresD (r :: ModifyingTagType) (a1 :: Unit) (a2 :: Unit) :: Unit where
    TresD 'None a1 a2 = (a1 / a2)
    TresD r a1 a2 = DropMultTag (a1 / a2)

type family TresS (r :: ModifyingTagType) (a1 :: Unit) :: Unit where
    TresS 'None a1 = a1
    TresS r a1 = DropMultTag a1

type Ttag (a :: Unit) = Strip (GetMultTag a)
type ReduxS (a1 :: Unit) (a2 :: Unit) = HasModifyingTag a1 `SelectMode` HasModifyingTag a2

class SINum (a1 :: Unit) (a2 :: Unit) b (c :: ModifyingTagType) where
    (*) :: (c ~ HasModifyingTag (a1 * a2)
         , SingI (Ttag (a1 * a2))) =>
        SI a1 b -> SI a2 b -> SI (TresM c a1 a2) b

    (+) :: (c ~ ReduxS a1 a2
         , (TresS c a1) ~ (TresS c a2)
         , SingI (Ttag a1)
         , SingI (Ttag a2)) =>
        SI a1 b -> SI a2 b -> SI (TresS c a1) b

    (-) :: (c ~ ReduxS a1 a2
         , (TresS c a1) ~ (TresS c a2)
         , SingI (Ttag a1)
         , SingI (Ttag a2)) =>
        SI a1 b -> SI a2 b -> SI (TresS c a1) b

instance P.Fractional b => SINum a1 a2 b 'Submultiple where
    (*) (SI l) (SI r) = SI (l P.* r P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a1 * a2))))
    (+) (SI l) (SI r) = SI (l P.* proxyL P.+ r P.* proxyR)
      where
        proxyL = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a1))))
        proxyR = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a2))))
    (-) (SI l) (SI r) = SI (l P.* proxyL P.- r P.* proxyR)
      where
        proxyL = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a1))))
        proxyR = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a2))))

instance P.Num b => SINum a1 a2 b 'Multiple where
    (*) (SI l) (SI r) = SI (l P.* r P.* proxy)
      where
        proxy = 10 P.^ toInt (sing :: Sing (Strip (GetMultTag (a1 * a2))))
    (+) (SI l) (SI r) = SI (l P.* proxyL P.+ r P.* proxyR)
      where
        proxyL = 10 P.^ toInt (sing :: Sing (Strip (GetMultTag (a1))))
        proxyR = 10 P.^ toInt (sing :: Sing (Strip (GetMultTag (a2))))
    (-) (SI l) (SI r) = SI (l P.* proxyL P.- r P.* proxyR)
      where
        proxyL = 10 P.^ toInt (sing :: Sing (Strip (GetMultTag (a1))))
        proxyR = 10 P.^ toInt (sing :: Sing (Strip (GetMultTag (a2))))

instance P.Num b => SINum a1 a2 b 'None where
    (*) (SI l) (SI r) = SI (l P.* r)
    (+) (SI l) (SI r) = SI (l P.+ r)
    (-) (SI l) (SI r) = SI (l P.- r)


class P.Fractional b => SIFractional (a1 :: Unit) (a2 :: Unit) b (c :: ModifyingTagType) where
    (/) :: (c ~ HasModifyingTag (a1 / a2)
         , SingI (Ttag (a1 / a2))) =>
        SI a1 b -> SI a2 b -> SI (TresD c a1 a2) b

instance P.Fractional b => SIFractional a1 a2 b 'Submultiple where
    (/) (SI l) (SI r) = SI (l P./ r P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a1 / a2))))

instance P.Fractional b => SIFractional a1 a2 b 'Multiple where
    (/) (SI l) (SI r) = SI (l P./ r P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (a1 / a2))))

instance P.Fractional b => SIFractional a1 a2 b 'None where
    (/) (SI l) (SI r) = SI (l P./ r)

type ExpResType (p :: Boolean) (a :: Unit) (e :: Exp) = If p (a ^ e) (I / a ^ e)
class P.Fractional b => SIExp (a :: Unit) (p :: Boolean) (e :: Exp) b (c :: ModifyingTagType) where
    (^) :: (c ~ HasModifyingTag (ExpResType p a e)
         , SingI (Ttag (ExpResType p a e))) =>
        SI a b -> Power p e -> SI (TresS c (ExpResType p a e)) b

instance P.Fractional b =>SIExp a p e b 'Submultiple where
    (^) (SI b) (Power n) = SI (b P.^^ n P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (ExpResType p a e))))

instance P.Fractional b =>SIExp a p e b 'Multiple where
    (^) (SI b) (Power n) = SI (b P.^^ n P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag (ExpResType p a e))))

instance P.Fractional b =>SIExp a p e b 'None where
    (^) (SI b) (Power n) = SI (b P.^^ n)

class P.Floating b => SISqrt (a :: Unit) b (c :: ModifyingTagType) where
    sqrt :: (c ~ HasModifyingTag (a ^ (P1 % P2))
         , SingI (Ttag a)) =>
        SI a b -> SI (TresS c (a ^ (P1 % P2))) b

instance P.Floating b => SISqrt a b 'Submultiple where
    sqrt (SI b) = SI (P.sqrt P.$! b P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag a)))

instance P.Floating b => SISqrt a b 'Multiple where
    sqrt (SI b) = SI (P.sqrt P.$! b P.* proxy)
      where
        proxy = 10 P.^^ toInt (sing :: Sing (Strip (GetMultTag a)))

instance P.Floating b => SISqrt a b 'None where
    sqrt (SI b) = SI (P.sqrt b)


deTag :: forall a b. (P.Num b, SingI (Strip (GetMultTag a))) =>
    SI a b -> SI (DropMultTag a) b
deTag (SI l) = SI (l P.* proxy)
  where
    proxy = 10 P.^ toInt (sing :: Sing (Strip (GetMultTag (a))))

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

infixr 8 **, ^^, ^

(**) :: P.Floating b => SI I b -> b -> SI I b
(**) (SI b) e = SI (b P.** e)

(^^) :: (P.Fractional a, P.Integral b) => SI I a -> b -> SI I a
(^^) (SI b) e = SI (b P.^^ e)

newtype Power (p :: Boolean) (e :: Exp) where
    Power :: P.Int -> Power p e

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

sum :: forall t a b. (P.Num b, P.Foldable t, HasModifyingTag a ~ 'None, SingI (Ttag a)) =>
    t (SI a b) -> SI a b
sum = P.foldr1 (+)

product :: forall t b. (P.Num b, P.Foldable t) => t (SI I b) -> SI I b
product = P.foldr (*) (SI 1 :: SI I b)
