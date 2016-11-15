{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
module SIUnits(Unit(..), SI(..)) where

import Numerals

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

infixl 7 :*:, :/:

data Gather where
    GU :: Unit -> Gather -> Gather
    GD :: Unit -> Gather -> Gather
    GZ :: Gather

data UnitList where
    UU :: Unit -> UnitList -> UnitList
    UZ :: UnitList

data Gather2 where
    G2 :: UnitList -> UnitList ->Gather2

type family Concat (a :: Gather) (b :: Gather) :: Gather where
    Concat GZ b = b
    Concat a GZ = a
    Concat (GU u rest) b = GU u (Concat rest b)
    Concat (GD u rest) b = GD u (Concat rest b)

type family ConcatUnits (a :: UnitList) (b :: UnitList) :: UnitList where
    ConcatUnits UZ b = b
    ConcatUnits a UZ = a
    ConcatUnits (UU u rest) b = UU u (ConcatUnits rest b)

type family Switch (op :: Unit -> Gather -> Gather) :: Unit -> Gather -> Gather where
    Switch GU = GD
    Switch GD = GU

type family Split (op :: Unit -> Gather -> Gather) (a :: Unit) :: Gather where
    Split op (a :*: b) = Concat (Split op a) (Split op b)
    Split op (a :/: b) = Concat (Split op a) (Split (Switch op) b)
    Split op a = op a GZ

type family Group (a :: Gather2) (b :: Gather) :: Gather2 where
    Group (G2 up down) (GU u rest) = Group (G2 (ConcatUnits up (UU u UZ)) down) rest
    Group (G2 up down) (GD u rest) = Group (G2 up (ConcatUnits down (UU u UZ))) rest
    Group grp GZ = grp

type family RemoveDups (a :: Gather2) :: Gather2 where
    RemoveDups (G2 up UZ) = G2 up UZ
    RemoveDups (G2 UZ down) = G2 UZ down
    RemoveDups (G2 up down) = G2 (Fold up down) (Fold down up)

type family Delete (a :: UnitList) (b :: Unit) :: UnitList where
    Delete UZ unit = UZ
    Delete (UU Kg rest) Kg = rest
    Delete (UU M rest) M = rest
    Delete (UU S rest) S = rest
    Delete (UU A rest) A = rest
    Delete (UU K rest) K = rest
    Delete (UU Mol rest) Mol = rest
    Delete (UU Cd rest) Cd = rest

    Delete (UU I rest) I = rest
    Delete (UU u rest) unit = UU u (Delete rest unit)

type family Fold (a :: UnitList) (b :: UnitList) :: UnitList where
    Fold z UZ = z
    Fold z (UU u rest) = Fold (Delete z u) rest

type family Normalize (a :: Gather2) :: Unit where
    Normalize (G2 up UZ) = Unwind up
    Normalize (G2 up down) = (Unwind up) :/: (Unwind down)

type family Unwind (a :: UnitList) :: Unit where
    Unwind (UU u rest) = u :*: Unwind rest
    Unwind UZ = I

type family Simplify (a :: Unit) :: Unit where
    Simplify I = I
    Simplify (a :/: I) = Simplify a
    Simplify (a :*: I) = Simplify a

    Simplify (a :*: b) = (Simplify a) :*: (Simplify b)
    Simplify (a :/: b) = (Simplify a) :/: (Simplify b)

    Simplify a = a

type family Mult (a :: Unit) (b :: Unit) :: Unit where
    Mult I b = b
    Mult a I = a
    Mult a b = Simplify (Normalize (RemoveDups (Group (G2 UZ UZ) (Split GU (a :*: b)))))

type family Div (a :: Unit) (b :: Unit) :: Unit where
    Div I b = b
    Div a I = a
    Div a b = Simplify (Normalize (RemoveDups (Group (G2 UZ UZ) (Split GU (a :/: b)))))

data SI (a :: Unit) b where
    SI :: b -> SI a b

deriving instance Show b => Show (SI a b)

instance Num b => Num (SI (a :: Unit) b) where
    -- (+) :: (Div a1 a2 ~ I) => SI a1 b -> SI a2 b -> SI a1 b
    (+) (SI l) (SI r) = SI $ l + r

--mu :: Num b => SI (a1 :: Unit) b -> SI (a2 :: Unit) b -> SI (Mult a1 a2) b
mu :: Num b => SI a1 b -> SI a2 b -> SI (Mult a1 a2) b
mu (SI l) (SI r) = SI $ l * r

su :: (Num b, Div a1 a2 ~ I) => SI a1 b -> SI a2 b -> SI a1 b
su (SI l) (SI r) = SI $ l + r

a :: SI (M :*: Kg) Int
a = SI 4

b :: SI (Kg :*: M) Int
b = SI 8

c = a `mu` b