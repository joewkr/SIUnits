{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Units.SI where

import qualified Prelude as P

import Numeric.Units.SI.Base
import Numeric.Units.SI.Numerals

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