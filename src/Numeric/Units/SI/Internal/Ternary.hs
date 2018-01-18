{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Numeric.Units.SI.Internal.Ternary where

import Data.Singletons.TH hiding (If, Min)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

$(singletons [d|
  data Ternary where
    TBot :: Ternary
    TZ :: Ternary -> Ternary
    T1 :: Ternary -> Ternary
    TJ :: Ternary -> Ternary deriving (Show, Lift)
  |])

ternary :: QuasiQuoter
ternary = QuasiQuoter {
    quoteDec  = error "Dec"
  , quoteExp  = error "Exp"
  , quotePat  = error "Pat"
  , quoteType = convertToTernary
}

convertToTernary :: String -> Q Type
convertToTernary str = do
    let converted = reverseTernary (PromotedT 'TBot) . toTernary TBot . read $ str
    return converted

toTernary :: Ternary -> Int -> Ternary
toTernary res 0 = res
toTernary res num
    | num `mod` 3 ==  2 = toTernary (TJ res) $! (num + 1) `div` 3
    | num `mod` 3 ==  1 = toTernary (T1 res) $! (num + 1) `div` 3
    | otherwise = toTernary (TZ res) $! (num + 1) `div` 3

reverseTernary :: Type -> Ternary -> Type
reverseTernary res TBot = res
reverseTernary res (TJ rest) = reverseTernary (AppT (PromotedT 'TJ) res) rest
reverseTernary res (T1 rest) = reverseTernary (AppT (PromotedT 'T1) res) rest
reverseTernary res (TZ rest) = reverseTernary (AppT (PromotedT 'TZ) res) rest