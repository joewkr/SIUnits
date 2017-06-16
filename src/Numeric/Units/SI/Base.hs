{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Base(Unit,
    Kg, M, S, A, K, Mol, Cd, I,
    Mult, NormalForm, Div, type(*), type(/), type(^)) where

import Numeric.Units.SI.Numerals

type Kg = 'Kg_
type M = 'M_
type S = 'S_
type A = 'A_
type K = 'K_
type Mol = 'Mol_
type Cd = 'Cd_
type I = 'I_

data Unit where
    Kg_ :: Unit -- kilogram
    M_ :: Unit -- metre
    S_ :: Unit -- second
    A_ :: Unit -- ampere
    K_ :: Unit -- kelvin
    Mol_ :: Unit -- mole
    Cd_ :: Unit -- candela
    I_ :: Unit -- dimensionless
    (:*:) :: Unit -> Unit -> Unit
    (:/:) :: Unit -> Unit -> Unit
    (:^:) :: Unit -> Exp -> Unit

type family (*) (a :: Unit) (b :: Unit) :: Unit where
    (*) a b = NormalForm (a ':*: b)
type family (/) (a :: Unit) (b :: Unit) :: Unit where
    (/) a b = NormalForm (a ':/: b)
type family (^) (a :: Unit) (b :: Exp) :: Unit where
    (^) a b = NormalForm (a ':^: b)

infixr 8 ^
infixl 7 *, /

infixr 8 :^:
infixl 7 :*:, :/:

data UnitsSpec where
    US :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> UnitsSpec

data Gather where
    GU :: Unit -> Gather -> Gather
    GZ :: Gather

type family Unwind (a :: Gather) :: Unit where
    Unwind 'GZ = I
    Unwind ('GU u rest) = u ':*: (Unwind rest)

type family Concat (a :: Gather) (b :: Gather) :: Gather where
    Concat 'GZ b = b
    Concat a 'GZ = a
    Concat ('GU u rest) b = 'GU u (Concat rest b)

type EmptyUS = 'US PZ PZ PZ PZ PZ PZ PZ
type Group (a :: Unit) = GroupQ EmptyUS a

type family GroupQ (a :: UnitsSpec) (b :: Unit) :: UnitsSpec where
    GroupQ spec (a ':*: rest) = GroupQ (CombineUntis spec a) rest
    GroupQ spec a = CombineUntis spec a

type family CombineUntis (a :: UnitsSpec) (b :: Unit) :: UnitsSpec where
    CombineUntis ('US kg m s a k mol cd) I = 'US kg m s a k mol cd

    CombineUntis ('US kg m s a k mol cd) (Kg  ':^: exp) = 'US (kg.+.exp) m s a k mol cd
    CombineUntis ('US kg m s a k mol cd) (M   ':^: exp) = 'US kg (m.+.exp) s a k mol cd
    CombineUntis ('US kg m s a k mol cd) (S   ':^: exp) = 'US kg m (s.+.exp) a k mol cd
    CombineUntis ('US kg m s a k mol cd) (A   ':^: exp) = 'US kg m s (a.+.exp) k mol cd
    CombineUntis ('US kg m s a k mol cd) (K   ':^: exp) = 'US kg m s a (k.+.exp) mol cd
    CombineUntis ('US kg m s a k mol cd) (Mol ':^: exp) = 'US kg m s a k (mol.+.exp) cd
    CombineUntis ('US kg m s a k mol cd) (Cd  ':^: exp) = 'US kg m s a k mol (cd.+.exp)

type family Switch (op :: Op) :: Op where
    Switch 'Plus = 'Minus
    Switch 'Minus = 'Plus

data Op where
    Plus :: Op
    Minus :: Op

type Split (a :: Unit) = Unwind (SplitQ 'Plus a)
type family SplitQ (op :: Op) (a :: Unit) :: Gather where
    SplitQ op (a ':*: b) = Concat (SplitQ op a) (SplitQ op b)
    SplitQ op (a ':/: b) = Concat (SplitQ op a) (SplitQ (Switch op) b)
    SplitQ op a = 'GU (Apply op a) 'GZ

type family Apply (op :: Op) (a :: Unit) :: Unit where
    Apply 'Minus (a ':^: exp) = a ':^: (Negate exp)
    Apply op a = a

type family Normalize (a :: UnitsSpec) :: Unit where
    Normalize ('US kg1 m1 s1 a1 k1 mol1 cd1) =
        (   Kg ':^: kg1
        ':*: M ':^: m1
        ':*: S ':^: s1
        ':*: A ':^: a1
        ':*: K ':^: k1
        ':*: Mol ':^: mol1
        ':*: Cd ':^: cd1)

type family Simplify (a :: Unit) :: Unit where
    Simplify I = I
    Simplify (a ':^: PZ) = I

    Simplify (a ':*: b) = CombineMult (Simplify a) (Simplify b)
    Simplify (a ':/: b) = CombineDiv (Simplify a) (Simplify b)
    Simplify (a ':^: exp) = CombineExp (Simplify a) exp

    Simplify a = a

type family CombineMult (a :: Unit) (b :: Unit) :: Unit where
    CombineMult I I = I
    CombineMult a I = a
    CombineMult I b = b
    CombineMult a b = a ':*: b

type family CombineDiv (a :: Unit) (b :: Unit) :: Unit where
    CombineDiv I I = I
    CombineDiv a I = a
    CombineDiv a b = a ':/: b

type family CombineExp (a :: Unit) (b :: Exp) :: Unit where
    CombineExp I e = I
    CombineExp a P1 = a
    CombineExp a e = a ':^: e

type family PutPowers (e :: Exp) (a :: Unit) :: Unit where
    PutPowers e I = I
    PutPowers e (a ':*: b) = PutPowers e a ':*: PutPowers e b
    PutPowers e (a ':/: b) = PutPowers e a ':/: PutPowers e b
    PutPowers e (a ':^: exp) = a ':^: (exp .*. e)

    PutPowers e a = a ':^: e

type family PropagateOuterPower (a :: Unit) :: Unit where
    PropagateOuterPower (a ':^: exp) = PutPowers exp a
    PropagateOuterPower a = PutPowers P1 a

type NormalForm (a :: Unit) = Simplify (Normalize (Group (Split (PropagateOuterPower a))))

type Mult a b = a * b
type Div a b = a / b
