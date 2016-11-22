{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Numeric.Units.SI.Base(Unit(..), SI(..), Mult, NormalForm, Div) where

import Control.DeepSeq
import GHC.Generics (Generic)

import Numeric.Units.SI.Numerals

data Unit where
    Kg :: Unit -- kilogram
    M :: Unit -- metre
    S :: Unit -- second
    A :: Unit -- ampere
    K :: Unit -- kelvin
    Mol :: Unit -- mole
    Cd :: Unit -- mole
    I :: Unit -- dimensionless
    (:*:) :: Unit -> Unit -> Unit
    (:/:) :: Unit -> Unit -> Unit
    (:^:) :: Unit -> Exp -> Unit

infixr 8 :^:
infixl 7 :*:, :/:

data UnitsSpec where
    US :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> UnitsSpec

data DimSpec where
    DS :: UnitsSpec -> UnitsSpec -> DimSpec

data Gather where
    GU :: Unit -> Gather -> Gather
    GD :: Unit -> Gather -> Gather
    GZ :: Gather

type family Concat (a :: Gather) (b :: Gather) :: Gather where
    Concat GZ b = b
    Concat a GZ = a
    Concat (GU u rest) b = GU u (Concat rest b)
    Concat (GD u rest) b = GD u (Concat rest b)

type family CombineUntis (a :: UnitsSpec) (b :: Unit) :: UnitsSpec where
    CombineUntis (US kg m s a k mol cd) I = US kg m s a k mol cd

    CombineUntis (US kg m s a k mol cd) (Kg  :^: exp) = US (kg.+.exp) m s a k mol cd
    CombineUntis (US kg m s a k mol cd) (M   :^: exp) = US kg (m.+.exp) s a k mol cd
    CombineUntis (US kg m s a k mol cd) (S   :^: exp) = US kg m (s.+.exp) a k mol cd
    CombineUntis (US kg m s a k mol cd) (A   :^: exp) = US kg m s (a.+.exp) k mol cd
    CombineUntis (US kg m s a k mol cd) (K   :^: exp) = US kg m s a (k.+.exp) mol cd
    CombineUntis (US kg m s a k mol cd) (Mol :^: exp) = US kg m s a k (mol.+.exp) cd
    CombineUntis (US kg m s a k mol cd) (Cd  :^: exp) = US kg m s a k mol (cd.+.exp)

type family Switch (op :: Unit -> Gather -> Gather) :: Unit -> Gather -> Gather where
    Switch GU = GD
    Switch GD = GU

type family Split (op :: Unit -> Gather -> Gather) (a :: Unit) :: Gather where
    Split op (a :*: b) = Concat (Split op a) (Split op b)
    Split op (a :/: b) = Concat (Split op a) (Split (Switch op) b)
    Split op a = op a GZ

type family Group (a :: DimSpec) (b :: Gather) :: DimSpec where
    Group (DS up down) (GU u rest) = Group (DS (CombineUntis up u) down) rest
    Group (DS up down) (GD u rest) = Group (DS up (CombineUntis down u)) rest
    Group grp GZ = grp

type family Dif1 (a :: Exp) (b :: Exp) :: Exp where
    Dif1 a b = If (a .>. b ) (a .-. b ) PZ

type family Dif2 (a :: Exp) (b :: Exp) :: Exp where
    Dif2 a b = If (a .>. b ) PZ (b .-. a)

type family If (c :: Boolean) (a :: Exp) (b :: Exp) :: Exp where
    If BT a b = a
    If BF a b = b

type family RemoveDups (a :: DimSpec) :: DimSpec where
    RemoveDups (DS (US kg1 m1 s1 a1 k1 mol1 cd1) (US kg2 m2 s2 a2 k2 mol2 cd2)) = DS
        (US (Dif1 kg1 kg2) (Dif1 m1 m2) (Dif1 s1 s2) (Dif1 a1 a2) (Dif1 k1 k2) (Dif1 mol1 mol2) (Dif1 cd1 cd2))
        (US (Dif2 kg1 kg2) (Dif2 m1 m2) (Dif2 s1 s2) (Dif2 a1 a2) (Dif2 k1 k2) (Dif2 mol1 mol2) (Dif2 cd1 cd2))

type family Normalize (a :: DimSpec) :: Unit where
    Normalize (DS (US kg1 m1 s1 a1 k1 mol1 cd1) (US kg2 m2 s2 a2 k2 mol2 cd2)) =
        (Kg :^: kg1 :*: M :^: m1 :*: S :^: s1 :*: A :^: a1 :*: K :^: k1 :*: Mol :^: mol1 :*: Cd :^: cd1) :/:
        (Kg :^: kg2 :*: M :^: m2 :*: S :^: s2 :*: A :^: a2 :*: K :^: k2 :*: Mol :^: mol2 :*: Cd :^: cd2)

type family Simplify (a :: Unit) :: Unit where
    Simplify I = I
    Simplify (a :^: PZ) = I

    Simplify (I :*: b) = Simplify b
    Simplify (I :^: exp) = I

    Simplify (a :/: I) = Simplify a
    Simplify (a :*: I) = Simplify a
    Simplify (a :^: P1) = Simplify a

    Simplify (a :*: b) = (Simplify a) :*: (Simplify b)
    Simplify (a :/: b) = (Simplify a) :/: (Simplify b)
    Simplify (a :^: exp) = (Simplify a) :^: exp

    Simplify a = a

type family PutPowers (a :: Unit) :: Unit where
    PutPowers I = I
    PutPowers (a :*: b) = (PutPowers a) :*: (PutPowers b)
    PutPowers (a :/: b) = (PutPowers a) :/: (PutPowers b)
    PutPowers (a :^: exp) = a :^: exp

    PutPowers a = a :^: P1

type EmptyUS = US PZ PZ PZ PZ PZ PZ PZ

type Simplify3 (a :: Unit) = Simplify (Simplify (Simplify a))
type NormalForm (a :: Unit) =
    Simplify3 (Normalize (RemoveDups (Group (DS EmptyUS EmptyUS) (Split GU (PutPowers a)))))

type family Mult (a :: Unit) (b :: Unit) :: Unit where
    Mult I b = b
    Mult a I = a
    Mult a b = NormalForm (a :*: b)

type family Div (a :: Unit) (b :: Unit) :: Unit where
    Div I b = b
    Div a I = a
    Div a b = NormalForm (a :/: b)

data SI (a :: Unit) b where
    SI :: !b -> SI a b deriving Generic

deriving instance Show b => Show (SI a b)

instance NFData b => NFData (SI a b)

instance Functor (SI a) where
    fmap f (SI val) = SI (f val)