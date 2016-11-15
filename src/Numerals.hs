{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numerals where

data Exp where
    PZ :: Exp
    PS :: Exp -> Exp
    (:%:) :: Exp -> Exp -> Exp

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

type Min (x :: Ternary) (y :: Ternary) = MinQ ((Abs y) - (Abs x)) x y TBot
type Min' (x :: Ternary) (y :: Ternary) = MinQ ((Abs (T1 y)) - (Abs (T1 x))) x y TBot
type Min'' (x :: Ternary) (y :: Ternary) = MinQ ((Abs (TJ y)) - (Abs (TJ x))) x y TBot

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
