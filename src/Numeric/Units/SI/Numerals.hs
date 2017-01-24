{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Numerals(Exp, Boolean(..), ComputeIrreducible,
    PZ, P1, P2, P3, P4, P5, P6, P7, P8, P9, type(%), type(.+.), type(.*.),
    type(.-.), type(.>.)) where

data Boolean where
    BT :: Boolean
    BF :: Boolean

type PZ = UZ :%: U1

type P1 = U1 :%: U1
type P2 = U2 :%: U1
type P3 = U3 :%: U1
type P4 = U4 :%: U1
type P5 = U5 :%: U1
type P6 = U6 :%: U1
type P7 = U7 :%: U1
type P8 = U8 :%: U1
type P9 = U9 :%: U1

data Exp where
    (:%:) :: Unary -> Unary -> Exp

type family (%) (x :: Exp) (y :: Exp) :: Exp where
    (%) x (y1 :%: y2) = x .*. (y2 :%: y1)


type family (.+.) (x :: Exp) (y :: Exp) :: Exp where
    (.+.) (x1 :%: x2) (y1 :%: y2) = ComputeIrreducible (((x1 ~*~ y2) ~+~ (x2 ~*~ y1)) :%: (x2 ~*~ y2))

type family (.-.) (x :: Exp) (y :: Exp) :: Exp where
    (.-.) (x1 :%: x2) (y1 :%: y2) = CheckZero (ComputeIrreducible (((x1 ~*~ y2) ~-~ (x2 ~*~ y1)) :%: (x2 ~*~ y2)))

type family (.*.) (x :: Exp) (y :: Exp) :: Exp where
    (.*.) (x1 :%: x2) (y1 :%: y2) = ComputeIrreducible ((x1 ~*~ y1) :%: (x2 ~*~ y2))

type family (.>.) (x :: Exp) (y :: Exp) :: Boolean where
    (.>.) (x1 :%: x2) (y1 :%: y2) = (x1 ~*~ y2) `Greather` (x2 ~*~ y1)

type family CheckZero (a :: Exp) :: Exp where
    CheckZero (UZ :%: n) = PZ
    CheckZero x = x

type family ComputeIrreducible (a :: Exp) :: Exp where
    ComputeIrreducible (x :%: y) = Reduce x y :%: Reduce y x

type Reduce (x :: Unary) (y :: Unary) = FromTernary (FI (ToTernary x) (ToTernary y))

type family Greather (a :: Unary) (b :: Unary) :: Boolean where
    Greather x UZ = BT
    Greather UZ y = BF
    Greather (US x) (US y) = x `Greather` y

data Unary where
    UZ :: Unary
    US :: Unary -> Unary

type U1 = US UZ
type U2 = US U1
type U3 = US U2
type U4 = US U3
type U5 = US U4
type U6 = US U5
type U7 = US U6
type U8 = US U7
type U9 = US U8

type family (~+~) (x :: Unary) (y :: Unary) :: Unary where
    (~+~) x UZ = x
    (~+~) x (US y) = US (x ~+~ y)

type family (~-~) (x :: Unary) (y :: Unary) :: Unary where
    (~-~) x UZ = x
    (~-~) (US x) (US y) = x ~-~ y

type family (~*~) (x :: Unary) (y :: Unary) :: Unary where
    (~*~) x UZ = UZ
    (~*~) x (US y) = x ~+~ (x ~*~ y)

type family ToTernary (a :: Unary) :: Ternary where
    ToTernary UZ = TBot
    ToTernary (US rest) = T1 TBot + ToTernary rest

type family FromTernary (a :: Ternary) :: Unary where
    FromTernary TBot = UZ
    FromTernary (TZ rest) = U3 ~*~ FromTernary rest
    FromTernary (T1 rest) = (U3 ~*~ FromTernary rest) ~+~ U1
    FromTernary (TJ rest) = (U3 ~*~ FromTernary rest) ~-~ U1

data Ternary where
    TBot :: Ternary
    TZ :: Ternary -> Ternary
    T1 :: Ternary -> Ternary
    TJ :: Ternary -> Ternary

type family Opp (a :: Ternary) :: Ternary where
    Opp TBot = TBot
    Opp (TZ rest) = TZ (Opp rest)
    Opp (T1 rest) = TJ (Opp rest)
    Opp (TJ rest) = T1 (Opp rest)

type family (+) (x :: Ternary) (y :: Ternary) :: Ternary where
    (+) x TBot = x
    (+) TBot y = y

    (+) (TZ x) (TZ y) = TZ (x + y)
    (+) (TZ x) (T1 y) = T1 (x + y)
    (+) (TZ x) (TJ y) = TJ (x + y)

    (+) (T1 x) (TZ y) = T1 (x + y)
    (+) (T1 x) (T1 y) = TJ (x + y + T1 TBot)
    (+) (T1 x) (TJ y) = TZ (x + y)

    (+) (TJ x) (TZ y) = TJ (x + y)
    (+) (TJ x) (T1 y) = TZ (x + y)
    (+) (TJ x) (TJ y) = T1 (x + y + TJ TBot)

type family (-) (x :: Ternary) (y :: Ternary) :: Ternary where
    (-) x y = x + Opp y

type Sign (x :: Ternary) = SignQ x TBot

type family SignQ (x :: Ternary) (y :: Ternary) :: Ternary where
    SignQ TBot y = y
    SignQ (TZ x) y = SignQ x y
    SignQ (T1 x) y = SignQ x (T1 TBot)
    SignQ (TJ x) y = SignQ x (TJ TBot)

type Abs (x :: Ternary) = AbsQ x x TBot

type family AbsQ (x :: Ternary) (y :: Ternary) (z ::Ternary) :: Ternary where
    AbsQ TBot y TBot = TBot
    AbsQ TBot y (T1 TBot) = y
    AbsQ TBot y (TJ TBot) = Opp y

    AbsQ (TZ x) y z = AbsQ x y z
    AbsQ (T1 x) y z = AbsQ x y (T1 TBot)
    AbsQ (TJ x) y z = AbsQ x y (TJ TBot)

type Min (x :: Ternary) (y :: Ternary) = MinQ (Abs y - Abs x) x y TBot
type Min' (x :: Ternary) (y :: Ternary) = MinQ (Abs (T1 y) - Abs (T1 x)) x y TBot
type Min'' (x :: Ternary) (y :: Ternary) = MinQ (Abs (TJ y) - Abs (TJ x)) x y TBot

type family MinQ (x :: Ternary) (y :: Ternary) (z :: Ternary) (u :: Ternary) :: Ternary where
    MinQ TBot y z TBot = y
    MinQ TBot y z (T1 TBot) = y
    MinQ TBot y z (TJ TBot) = z

    MinQ (TZ x) y z u = MinQ x y z u
    MinQ (T1 x) y z u = MinQ x y z (T1 TBot)
    MinQ (TJ x) y z u = MinQ x y z (TJ TBot)

type family FI (x :: Ternary) (y :: Ternary) :: Ternary where
    FI TBot y = TBot
    FI x TBot = Sign x

    FI (TZ x) (TZ y) = FI x y
    FI (TZ x) (T1 y) = TZ (FI x (T1 y))
    FI (TZ x) (TJ y) = TZ (FI x (TJ y))

    FI (T1 x) (TZ y) = FI (T1 x) y
    FI (T1 x) (T1 y) = TZ (FI (x - Min' x y) (T1 y)) + FI (Min (T1 x) (T1 y)) (x - y)
    FI (T1 x) (TJ y) = TZ (FI (x + Min'' (Opp x) y) (TJ y)) + FI (Min (T1 x) (T1 (Opp y))) (x + y)

    FI (TJ x) (TZ y) = FI (TJ x) y
    FI (TJ x) (T1 y) = TZ (FI (x + Min' (Opp x) y) (T1 y)) + FI (Min (TJ x) (TJ (Opp y))) (x + y)
    FI (TJ x) (TJ y) = TZ (FI (x - Min'' x y) (TJ y)) + FI (Min (TJ x) (TJ y)) (x - y)
