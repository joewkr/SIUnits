{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Internal.Numerals(Exp, Boolean(..), If, ComputeIrreducible, Negate,
    PZ,
    P1, P2, P3, P4, P5, P6, P7, P8, P9,
    M1, M2, M3, M4, M5, M6, M7, M8, M9,
    type(%), type(.+.), type(.*.), type(.-.), type(.>.)) where

data Boolean where
    BT :: Boolean
    BF :: Boolean

type family If (c :: Boolean) (a :: k) (b :: k) :: k where
    If 'BT a b = a
    If 'BF a b = b

type M9 = (Opp TN9) ':%: TN1
type M8 = (Opp TN8) ':%: TN1
type M7 = (Opp TN7) ':%: TN1
type M6 = (Opp TN6) ':%: TN1
type M5 = (Opp TN5) ':%: TN1
type M4 = (Opp TN4) ':%: TN1
type M3 = (Opp TN3) ':%: TN1
type M2 = (Opp TN2) ':%: TN1
type M1 = (Opp TN1) ':%: TN1

type PZ = 'TBot ':%: TN1

type P1 = TN1 ':%: TN1
type P2 = TN2 ':%: TN1
type P3 = TN3 ':%: TN1
type P4 = TN4 ':%: TN1
type P5 = TN5 ':%: TN1
type P6 = TN6 ':%: TN1
type P7 = TN7 ':%: TN1
type P8 = TN8 ':%: TN1
type P9 = TN9 ':%: TN1

data Exp where
    (:%:) :: Ternary -> Ternary -> Exp

type Negate (x :: Exp) = x .*. ((Opp TN1) ':%: TN1)

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
    CheckZero ('TBot ':%: n) = PZ
    CheckZero x = x

type family CheckNegative (a :: Exp) :: Exp where
    CheckNegative (a ':%: b) = If (b `Greather` TN0) (a ':%: b) ((Opp a) ':%: (Opp b))

type family ComputeIrreducible (a :: Exp) :: Exp where
    ComputeIrreducible (x ':%: y) = CheckNegative (Reduce x y ':%: Reduce y x)

type Reduce (x :: Ternary) (y :: Ternary) = TReverse (TStrip (TReverse (FI x  y)))

type family TStrip (x :: Ternary) :: Ternary where
    TStrip 'TBot = 'TBot
    TStrip ('TZ x) = TStrip x
    TStrip x = x

type Greather (a :: Ternary) (b :: Ternary) = GreatherQ (TReverse (a - b))

type family GreatherQ (x :: Ternary) :: Boolean where
    GreatherQ ('T1 x) = 'BT
    GreatherQ x = 'BF

type TN0 = 'TZ 'TBot
type TN1 = 'T1 'TBot
type TN2 = TN1 + TN1
type TN3 = TN2 + TN1
type TN4 = TN3 + TN1
type TN5 = TN4 + TN1
type TN6 = TN5 + TN1
type TN7 = TN6 + TN1
type TN8 = TN7 + TN1
type TN9 = TN8 + TN1

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

type (+) (x :: Ternary) (y :: Ternary) = x `TSum` y
type (-) (x :: Ternary) (y :: Ternary) = x `TSub` y
type (*) (x :: Ternary) (y :: Ternary) = x `TMult` y

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

type family TMult (x :: Ternary) (y :: Ternary) :: Ternary where
    TMult x 'TBot = 'TBot
    TMult x y = (TMultH x y) + (TMult ('TZ x) (TTail y))

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
