{-# LANGUAGE TypeOperators #-}
module Numeric.Units.SI.Tags.Multiply.Prefixes(
    Deca, Hecto, Kilo, Mega, Giga, Tera, Peta, Exa, Zetta, Yotta,
    Deci, Centi, Milli, Micro, Nano, Pico, Femto, Atto, Zepto, Yocto
    ) where

import Numeric.Units.SI.Base
import Numeric.Units.SI.Internal.Numerals

type M24 = M21 .-. P3
type M21 = M18 .-. P3
type M18 = M15 .-. P3
type M15 = M12 .-. P3
type M12 = M9  .-. P3

type P12 = P9  .+. P3
type P15 = P12 .+. P3
type P18 = P15 .+. P3
type P21 = P18 .+. P3
type P24 = P21 .+. P3

type Deca u = MultiplyExp P1 u
type Hecto u = MultiplyExp P2 u
type Kilo u = MultiplyExp P3 u
type Mega u = MultiplyExp P6 u
type Giga u = MultiplyExp P9 u
type Tera u = MultiplyExp P12 u
type Peta u = MultiplyExp P15 u
type Exa u = MultiplyExp P18 u
type Zetta u = MultiplyExp P21 u
type Yotta u = MultiplyExp P24 u

type Deci u = MultiplyExp M1 u
type Centi u = MultiplyExp M2 u
type Milli u = MultiplyExp M3 u
type Micro u = MultiplyExp M6 u
type Nano u = MultiplyExp M9 u
type Pico u = MultiplyExp M12 u
type Femto u = MultiplyExp M15 u
type Atto u = MultiplyExp M18 u
type Zepto u = MultiplyExp M21 u
type Yocto u = MultiplyExp M24 u