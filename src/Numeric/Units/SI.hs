{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.Units.SI(
      module Numeric.Units.SI.Base
    , module Numeric.Units.SI.Derived
    , SI(..)
    , (*), (/), (+), (-)
    , (**), (^^)
    , pi
    , exp, log
    , sin, cos, tan, asin, acos, atan
    , sinh, cosh, tanh, asinh, acosh, atanh
    , sqrt ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Prelude as P

import Numeric.Units.SI.Base
import Numeric.Units.SI.Derived
import Numeric.Units.SI.Numerals

data SI (a :: Unit) b where
    SI :: !b -> SI a b deriving Generic

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

(+) :: (P.Num b, NormalForm a1 ~ NormalForm a2) => SI a1 b -> SI a2 b -> SI a1 b
(+) (SI l) (SI r) = SI (l P.+ r)

(-) :: (P.Num b, NormalForm a1 ~ NormalForm a2) => SI a1 b -> SI a2 b -> SI a1 b
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

sqrt :: P.Floating b => SI a b -> SI (NormalForm (a :^: (P1 :%: P2))) b
sqrt (SI b) = SI (P.sqrt b)

(**) :: P.Floating b => SI I b -> b -> SI I b
(**) (SI b) e = SI (b P.** e)

(^^) :: (P.Fractional a, P.Integral b) => SI I a -> b -> SI I a
(^^) (SI b) e = SI (b P.^^ e)