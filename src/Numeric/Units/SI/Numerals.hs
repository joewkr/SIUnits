{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Numerals(Exp, Boolean(..), If, ComputeIrreducible,
    PZ, P1, P2, P3, P4, P5, P6, P7, P8, P9, type(%), type(.+.), type(.*.),
    type(.-.), type(.>.)) where

data Boolean where
    BT :: Boolean
    BF :: Boolean

type family If (c :: Boolean) (a :: k) (b :: k) :: k where
    If 'BT a b = a
    If 'BF a b = b

type PZ = 'UZ ':%: U1

type P1 = U1 ':%: U1
type P2 = U2 ':%: U1
type P3 = U3 ':%: U1
type P4 = U4 ':%: U1
type P5 = U5 ':%: U1
type P6 = U6 ':%: U1
type P7 = U7 ':%: U1
type P8 = U8 ':%: U1
type P9 = U9 ':%: U1

data Exp where
    (:%:) :: Unary -> Unary -> Exp

type family (%) (x :: Exp) (y :: Exp) :: Exp where
    (%) x (y1 ':%: y2) = x .*. (y2 ':%: y1)


type family (.+.) (x :: Exp) (y :: Exp) :: Exp where
    (.+.) (x1 ':%: x2) (y1 ':%: y2) = ComputeIrreducible (((x1 * y2) + (x2 * y1)) ':%: (x2 * y2))

type family (.-.) (x :: Exp) (y :: Exp) :: Exp where
    (.-.) (x1 ':%: x2) (y1 ':%: y2) = CheckZero (ComputeIrreducible (((x1 * y2) - (x2 * y1)) ':%: (x2 * y2)))

type family (.*.) (x :: Exp) (y :: Exp) :: Exp where
    (.*.) (x1 ':%: x2) (y1 ':%: y2) = ComputeIrreducible ((x1 * y1) ':%: (x2 * y2))

type family (.>.) (x :: Exp) (y :: Exp) :: Boolean where
    (.>.) (x1 ':%: x2) (y1 ':%: y2) = (x1 * y2) `Greather` (x2 * y1)

type family CheckZero (a :: Exp) :: Exp where
    CheckZero ('UZ ':%: n) = PZ
    CheckZero x = x

type family ComputeIrreducible (a :: Exp) :: Exp where
    ComputeIrreducible (x ':%: y) = Reduce x y ':%: Reduce y x

type Reduce (x :: Unary) (y :: Unary) = FromTernary (FI (ToTernary x) (ToTernary y))

type family Greather (a :: Unary) (b :: Unary) :: Boolean where
    Greather x 'UZ = 'BT
    Greather 'UZ y = 'BF
    Greather ('US x) ('US y) = x `Greather` y

data Unary where
    UZ :: Unary
    US :: Unary -> Unary

type U1 = 'US 'UZ
type U2 = 'US  U1
type U3 = 'US  U2
type U4 = 'US  U3
type U5 = 'US  U4
type U6 = 'US  U5
type U7 = 'US  U6
type U8 = 'US  U7
type U9 = 'US  U8

type family USum (x :: Unary) (y :: Unary) :: Unary where
    USum x 'UZ = x
    USum x ('US y) = 'US (x `USum` y)

type family USub (x :: Unary) (y :: Unary) :: Unary where
    USub x 'UZ = x
    USub ('US x) ('US y) = x `USub` y

type family UMul (x :: Unary) (y :: Unary) :: Unary where
    UMul x 'UZ = 'UZ
    UMul x ('US y) = x + (x `UMul` y)

type family ToTernary (a :: Unary) :: Ternary where
    ToTernary 'UZ = 'TBot
    ToTernary ('US rest) = 'T1 'TBot + ToTernary rest

type family FromTernary (a :: Ternary) :: Unary where
    FromTernary 'TBot = 'UZ
    FromTernary ('TZ rest) = U3 * FromTernary rest
    FromTernary ('T1 rest) = (U3 * FromTernary rest) + U1
    FromTernary ('TJ rest) = (U3 * FromTernary rest) - U1

data Ternary where
    TBot :: Ternary
    TZ :: Ternary -> Ternary
    T1 :: Ternary -> Ternary
    TJ :: Ternary -> Ternary

type family Opp (a :: Ternary) :: Ternary where
    Opp 'TBot = 'TBot
    Opp ('TZ rest) = 'TZ (Opp rest)
    Opp ('T1 rest) = 'TJ (Opp rest)
    Opp ('TJ rest) = 'T1 (Opp rest)

type family (+) (a :: k) (b :: k) :: k where
    (+) 'TBot   y = 'TBot   `TSum` y
    (+) ('TZ x) y = ('TZ x) `TSum` y
    (+) ('T1 x) y = ('T1 x) `TSum` y
    (+) ('TJ x) y = ('TJ x) `TSum` y

    (+) 'UZ     y = 'UZ     `USum` y
    (+) ('US x) y = ('US x) `USum` y

type family (-) (a :: k) (b :: k) :: k where
    (-) 'TBot   y = 'TBot   `TSub` y
    (-) ('TZ x) y = ('TZ x) `TSub` y
    (-) ('T1 x) y = ('T1 x) `TSub` y
    (-) ('TJ x) y = ('TJ x) `TSub` y

    (-) 'UZ     y = 'UZ     `USub` y
    (-) ('US x) y = ('US x) `USub` y

type family (*) (a :: k) (b :: k) :: k where
    (*) 'UZ     y = 'UZ     `UMul` y
    (*) ('US x) y = ('US x) `UMul` y

type family TSum (x :: Ternary) (y :: Ternary) :: Ternary where
    TSum x 'TBot = x
    TSum 'TBot y = y

    TSum ('TZ x) ('TZ y) = 'TZ (x `TSum` y)
    TSum ('TZ x) ('T1 y) = 'T1 (x `TSum` y)
    TSum ('TZ x) ('TJ y) = 'TJ (x `TSum` y)

    TSum ('T1 x) ('TZ y) = 'T1 (x `TSum` y)
    TSum ('T1 x) ('T1 y) = 'TJ (x `TSum` y `TSum` 'T1 'TBot)
    TSum ('T1 x) ('TJ y) = 'TZ (x `TSum` y)

    TSum ('TJ x) ('TZ y) = 'TJ (x `TSum` y)
    TSum ('TJ x) ('T1 y) = 'TZ (x `TSum` y)
    TSum ('TJ x) ('TJ y) = 'T1 (x `TSum` y `TSum` 'TJ 'TBot)

type family TSub (x :: Ternary) (y :: Ternary) :: Ternary where
    TSub x y = x + Opp y

type TReverse (x :: Ternary) = TReverseQ 'TBot x

type family TReverseQ (x :: Ternary) (y :: Ternary) :: Ternary where
    TReverseQ x 'TBot = x
    TReverseQ x ('T1 y) = TReverseQ ('T1 x) y
    TReverseQ x ('TZ y) = TReverseQ ('TZ x) y
    TReverseQ x ('TJ y) = TReverseQ ('TJ x) y

type family TTail (x :: Ternary) :: Ternary where
    TTail 'TBot = 'TBot
    TTail ('T1 x) = x
    TTail ('TZ x) = x
    TTail ('TJ x) = x

type TMult (x :: Ternary) (y :: Ternary) = TMultQ x y

type family TMultQ (x :: Ternary) (y :: Ternary) :: Ternary where
    TMultQ x 'TBot = 'TBot
    TMultQ x y = (TMultH x y) + (TMultQ ('TZ x) (TTail y))

type family TMultH (x :: Ternary) (y :: Ternary) :: Ternary where
    TMultH 'TBot y = 'TBot

    TMultH ('T1 x) ('T1 y) = 'T1 (TMultH x ('T1 y))
    TMultH ('T1 x) ('TJ y) = 'TJ (TMultH x ('TJ y))

    TMultH ('TJ x) ('T1 y) = 'TJ (TMultH x ('T1 y))
    TMultH ('TJ x) ('TJ y) = 'T1 (TMultH x ('TJ y))

    TMultH x y = 'TZ (TMultH (TTail x) y)

type Sign (x :: Ternary) = SignQ x 'TBot

type family SignQ (x :: Ternary) (y :: Ternary) :: Ternary where
    SignQ 'TBot y = y
    SignQ ('TZ x) y = SignQ x y
    SignQ ('T1 x) y = SignQ x ('T1 'TBot)
    SignQ ('TJ x) y = SignQ x ('TJ 'TBot)

type Abs (x :: Ternary) = AbsQ x x 'TBot

type family AbsQ (x :: Ternary) (y :: Ternary) (z ::Ternary) :: Ternary where
    AbsQ 'TBot y 'TBot = 'TBot
    AbsQ 'TBot y ('T1 'TBot) = y
    AbsQ 'TBot y ('TJ 'TBot) = Opp y

    AbsQ ('TZ x) y z = AbsQ x y z
    AbsQ ('T1 x) y z = AbsQ x y ('T1 'TBot)
    AbsQ ('TJ x) y z = AbsQ x y ('TJ 'TBot)

type Min (x :: Ternary) (y :: Ternary) = MinQ (Abs y - Abs x) x y 'TBot
type Min' (x :: Ternary) (y :: Ternary) = MinQ (Abs ('T1 y) - Abs ('T1 x)) x y 'TBot
type Min'' (x :: Ternary) (y :: Ternary) = MinQ (Abs ('TJ y) - Abs ('TJ x)) x y 'TBot

type family MinQ (x :: Ternary) (y :: Ternary) (z :: Ternary) (u :: Ternary) :: Ternary where
    MinQ 'TBot y z 'TBot = y
    MinQ 'TBot y z ('T1 'TBot) = y
    MinQ 'TBot y z ('TJ 'TBot) = z

    MinQ ('TZ x) y z u = MinQ x y z u
    MinQ ('T1 x) y z u = MinQ x y z ('T1 'TBot)
    MinQ ('TJ x) y z u = MinQ x y z ('TJ 'TBot)

type family FI (x :: Ternary) (y :: Ternary) :: Ternary where
    FI 'TBot y = 'TBot
    FI x 'TBot = Sign x

    FI ('TZ x) ('TZ y) = FI x y
    FI ('TZ x) ('T1 y) = 'TZ (FI x ('T1 y))
    FI ('TZ x) ('TJ y) = 'TZ (FI x ('TJ y))

    FI ('T1 x) ('TZ y) = FI ('T1 x) y
    FI ('T1 x) ('T1 y) = 'TZ (FI (x - Min' x y) ('T1 y)) + FI (Min ('T1 x) ('T1 y)) (x - y)
    FI ('T1 x) ('TJ y) = 'TZ (FI (x + Min'' (Opp x) y) ('TJ y)) + FI (Min ('T1 x) ('T1 (Opp y))) (x + y)

    FI ('TJ x) ('TZ y) = FI ('TJ x) y
    FI ('TJ x) ('T1 y) = 'TZ (FI (x + Min' (Opp x) y) ('T1 y)) + FI (Min ('TJ x) ('TJ (Opp y))) (x + y)
    FI ('TJ x) ('TJ y) = 'TZ (FI (x - Min'' x y) ('TJ y)) + FI (Min ('TJ x) ('TJ y)) (x - y)
