{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Derived where

import Numerals
import SIUnits

type Herz = I :/: S

type Newton = Kg :*: M :/: S :^: P2

type Pascal = Newton :/: M :^: P2

type Joule = Newton :*: M

type Watt = Joule :/: S

type Coulomb = S :*: A

type Volt = Watt :/: A

type Farad = Coulomb :/: Volt

type Ohm = Volt :/: A

type Siemens = I :/: Ohm

type Weber = Joule :/: A

type Tesla = Weber :/: M :^: P2

type Henry = Weber :/: A

type Lux = Cd :/: M :^: P2

type Becquerel = I :/: S

type Gray = Joule :/: Kg

type Sievert = Joule :/: Kg

type Katal = Mol :/: S
