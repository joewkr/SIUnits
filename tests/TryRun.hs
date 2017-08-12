{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
module Main(main) where

import Prelude hiding ((*))
import Test.Hspec

import Numeric.Units.SI

main :: IO ()
main = hspec $ do
    describe "Multiple prefixes" $ do
        it "resolves Deca" $ do
            deTag (SI 1 :: SI (Deca M) Integer)  `shouldBe` (SI 10 :: SI M Integer)
        it "resolves Hecto" $ do
            deTag (SI 1 :: SI (Hecto M) Integer) `shouldBe` (SI 100 :: SI M Integer)
        it "resolves Kilo" $ do
            deTag (SI 1 :: SI (Kilo M) Integer)  `shouldBe` (SI 1000 :: SI M Integer)
        it "resolves Mega" $ do
            deTag (SI 1 :: SI (Mega M) Integer)  `shouldBe` (SI 1000000 :: SI M Integer)
        it "resolves Giga" $ do
            deTag (SI 1 :: SI (Giga M) Integer)  `shouldBe` (SI 1000000000 :: SI M Integer)
        it "resolves Tera" $ do
            deTag (SI 1 :: SI (Tera M) Integer)  `shouldBe` (SI 1000000000000 :: SI M Integer)
        it "resolves Peta" $ do
            deTag (SI 1 :: SI (Peta M) Integer)  `shouldBe` (SI 1000000000000000 :: SI M Integer)
        it "resolves Exa" $ do
            deTag (SI 1 :: SI (Exa M) Integer)   `shouldBe` (SI 1000000000000000000 :: SI M Integer)
        it "resolves Zetta" $ do
            deTag (SI 1 :: SI (Zetta M) Integer) `shouldBe` (SI 1000000000000000000000 :: SI M Integer)
        it "resolves Yotta" $ do
            deTag (SI 1 :: SI (Yotta M) Integer) `shouldBe` (SI 1000000000000000000000000 :: SI M Integer)
    describe "Submultiple prefixes" $ do
        it "resolves Deci" $ do
            ((SI 1 :: SI (Deca M) Int) *
             (SI 1 :: SI (Deci M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Centi" $ do
            ((SI 1 :: SI (Hecto M) Int) *
             (SI 1 :: SI (Centi M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Milli" $ do
            ((SI 1 :: SI (Kilo M) Int) *
             (SI 1 :: SI (Milli M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Micro" $ do
            ((SI 1 :: SI (Mega M) Int) *
             (SI 1 :: SI (Micro M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Nano" $ do
            ((SI 1 :: SI (Giga M) Int) *
             (SI 1 :: SI (Nano M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Pico" $ do
            ((SI 1 :: SI (Tera M) Int) *
             (SI 1 :: SI (Pico M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Femto" $ do
            ((SI 1 :: SI (Peta M) Int) *
             (SI 1 :: SI (Femto M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Atto" $ do
            ((SI 1 :: SI (Exa M) Int) *
             (SI 1 :: SI (Atto M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Zepto" $ do
            ((SI 1 :: SI (Zetta M) Int) *
             (SI 1 :: SI (Zepto M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)
        it "resolves Yocto" $ do
            ((SI 1 :: SI (Yotta M) Int) *
             (SI 1 :: SI (Yocto M) Int)) `shouldBe` (SI 1 :: SI (M ^ P2) Int)