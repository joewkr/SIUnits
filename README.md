# SIUnits
[![Build Status](https://travis-ci.org/joewkr/SIUnits.svg?branch=master)](https://travis-ci.org/joewkr/SIUnits)

A lightweight library to handle units of measure on type-level.

## Usage

```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ThermalBalance where

import Numeric.Units.SI

import Prelude hiding((^), (*), (-))

thermalBalance :: SI (Watt / M ^ P2) Double
thermalBalance = downwardThermalRadiation - greyBodyIrradiance temperature

downwardThermalRadiation :: SI (Watt / M ^ P2) Double
downwardThermalRadiation = SI 300.0

temperature :: SI K Double
temperature = SI 273.15

greyBodyIrradiance :: SI K Double -> SI (Watt / M ^ P2) Double
greyBodyIrradiance t = emissivity*sigma*t^p4
  where
    emissivity :: SI I Double
    emissivity = SI 0.99

    sigma :: SI (Watt / (M ^ P2 * K ^ P4)) Double
    sigma = SI 5.670373E-8
```