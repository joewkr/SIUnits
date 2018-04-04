{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Base(Unit,
    Kg, Gram, M, S, A, K, Mol, Cd, I,
    MultiplyExp, HasMultTag, GetMultTag, DropMultTag, HasReducingMultTag,
    Mult, NormalForm, Div, type(*), type(/), type(^)) where

import Numeric.Units.SI.Internal.Numerals
import Numeric.Units.SI.Internal.Boolean

import GHC.TypeLits (TypeError, ErrorMessage(Text))

type Kg = 'Kg_
type M = 'M_
type S = 'S_
type A = 'A_
type K = 'K_
type Mol = 'Mol_
type Cd = 'Cd_
type I = 'I_

type Gram = 'Kg_ ':*: 'Tag ('Multiply M3)

data Unit where
    Kg_ :: Unit -- kilogram
    M_ :: Unit -- metre
    S_ :: Unit -- second
    A_ :: Unit -- ampere
    K_ :: Unit -- kelvin
    Mol_ :: Unit -- mole
    Cd_ :: Unit -- candela
    I_ :: Unit -- dimensionless
    Tag :: TagType -> Unit
    (:*:) :: Unit -> Unit -> Unit
    (:/:) :: Unit -> Unit -> Unit
    (:^:) :: Unit -> Exp -> Unit

data TagType where
    Multiply :: Exp -> TagType

--type MultiplyExp e u = 'Tag ('Multiply e) ':*: u

type family MultiplyExp (e :: Exp) (u :: Unit) where
    MultiplyExp e ('Tag ('Multiply t) ':*: rest) =
        TypeError ('Text "SI prefix cannot be repeated")
    MultiplyExp e Kg =
        TypeError ('Text "SI prefix cannot be applied to Kg unit")
    MultiplyExp e (Kg ':^: p) =
        TypeError ('Text "SI prefix cannot be applied to Kg unit")
    MultiplyExp e u = NormalForm ('Tag ('Multiply e) ':*: u)


type MultiplyZero = 'Multiply PZ

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
    US :: TagType -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> UnitsSpec

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

type EmptyUS = 'US MultiplyZero PZ PZ PZ PZ PZ PZ PZ
type Group (a :: Unit) = GroupQ EmptyUS a

type family GroupQ (a :: UnitsSpec) (b :: Unit) :: UnitsSpec where
    GroupQ spec (a ':*: rest) = GroupQ (CombineUntis spec a) rest
    GroupQ spec a = CombineUntis spec a

type family GetSpec (spec :: UnitsSpec) (u :: Unit) :: Exp where
    GetSpec ('US tag kg m s a k mol cd) Kg  = kg
    GetSpec ('US tag kg m s a k mol cd) M   = m
    GetSpec ('US tag kg m s a k mol cd) S   = s
    GetSpec ('US tag kg m s a k mol cd) A   = a
    GetSpec ('US tag kg m s a k mol cd) K   = k
    GetSpec ('US tag kg m s a k mol cd) Mol = mol
    GetSpec ('US tag kg m s a k mol cd) Cd  = cd

type family UpdateSpec (spec :: UnitsSpec) (u :: Unit) (v :: Exp) :: UnitsSpec where
    UpdateSpec ('US tag kg m s a k mol cd) Kg  val = 'US tag val m   s   a   k   mol cd
    UpdateSpec ('US tag kg m s a k mol cd) M   val = 'US tag kg  val s   a   k   mol cd
    UpdateSpec ('US tag kg m s a k mol cd) S   val = 'US tag kg  m   val a   k   mol cd
    UpdateSpec ('US tag kg m s a k mol cd) A   val = 'US tag kg  m   s   val k   mol cd
    UpdateSpec ('US tag kg m s a k mol cd) K   val = 'US tag kg  m   s   a   val mol cd
    UpdateSpec ('US tag kg m s a k mol cd) Mol val = 'US tag kg  m   s   a   k   val cd
    UpdateSpec ('US tag kg m s a k mol cd) Cd  val = 'US tag kg  m   s   a   k   mol val

type family CombineUntis (a :: UnitsSpec) (b :: Unit) :: UnitsSpec where
    CombineUntis spec I = spec

    CombineUntis spec ('Tag t ':^: exp) = HandleTags spec t exp

    CombineUntis spec (u  ':^: exp) = UpdateSpec spec u ((GetSpec spec u).+.exp)

type family HandleTags (a :: UnitsSpec) (b :: TagType) (e :: Exp) :: UnitsSpec where
    HandleTags ('US ('Multiply tag) kg m s a k mol cd) ('Multiply factor) exp =
        ('US ('Multiply (tag.+.factor.*.exp)) kg m s a k mol cd)

type Split (a :: Unit) = Unwind (SplitQ 'False a)
type family SplitQ (switch :: Bool) (a :: Unit) :: Gather where
    SplitQ switch (a ':*: b) = Concat (SplitQ switch a) (SplitQ switch b)
    SplitQ switch (a ':/: b) = Concat (SplitQ switch a) (SplitQ (Not switch) b)
    SplitQ switch a = 'GU (Apply switch a) 'GZ

type family Apply (switch :: Bool) (a :: Unit) :: Unit where
    Apply 'True (a ':^: exp) = a ':^: (Negate exp)
    Apply switch a = a

type family Normalize (a :: UnitsSpec) :: Unit where
    Normalize ('US tag kg1 m1 s1 a1 k1 mol1 cd1) =
        ('Tag tag
        ':*: (Kg ':^: kg1
        ':*: M ':^: m1
        ':*: S ':^: s1
        ':*: A ':^: a1
        ':*: K ':^: k1
        ':*: Mol ':^: mol1
        ':*: Cd ':^: cd1))

type family Simplify (a :: Unit) :: Unit where
    Simplify I = I

    Simplify ('Tag MultiplyZero ':*: b) = Simplify b
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
    CombineExp a e = CheckReduced a (ComputeIrreducible e)

type family CheckReduced (a :: Unit) (b :: Exp) :: Unit where
    CheckReduced a P1 = a
    CheckReduced a PZ = I
    CheckReduced a e = a ':^: e

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

type family HasMultTag (a :: Unit) :: Bool where
    HasMultTag ('Tag ('Multiply t) ':*: rest) = 'True
    HasMultTag otherwise = 'False

type family HasReducingMultTag (a :: Unit) :: Bool where
    HasReducingMultTag u = PZ .>. GetMultTag u

type family GetMultTag (a :: Unit) :: Exp where
    GetMultTag ('Tag ('Multiply t) ':*: rest) = t
    GetMultTag otherwise = PZ

type family DropMultTag (a :: Unit) :: Unit where
    DropMultTag ('Tag ('Multiply t) ':*: rest) = rest
    DropMultTag u = u

type Mult a b = a * b
type Div a b = a / b
