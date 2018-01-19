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
    TJ :: Ternary -> Ternary deriving Show
  |])

ternary :: QuasiQuoter
ternary = QuasiQuoter {
    quoteDec  = error errorMessage
  , quoteExp  = error errorMessage
  , quotePat  = error errorMessage
  , quoteType = convertToTernary }
  where
    errorMessage :: String
    errorMessage = "ternary QuasiQuoter can be used only in type context"


convertToTernary :: String -> Q Type
convertToTernary str = do
    let converted = buildTernaryTH (PromotedT 'TBot) . toTernary TBot . read $ str
    return converted

toTernary :: Ternary -> Int -> Ternary
toTernary res 0 = res
toTernary res num
    | num `mod` 3 ==  2 = toTernary (TJ res) $! (num + 1) `div` 3
    | num `mod` 3 ==  1 = toTernary (T1 res) $! (num + 1) `div` 3
    | otherwise = toTernary (TZ res) $! (num + 1) `div` 3

buildTernaryTH :: Type -> Ternary -> Type
buildTernaryTH res TBot = res
buildTernaryTH res (TJ rest) = buildTernaryTH (AppT (PromotedT 'TJ) res) rest
buildTernaryTH res (T1 rest) = buildTernaryTH (AppT (PromotedT 'T1) res) rest
buildTernaryTH res (TZ rest) = buildTernaryTH (AppT (PromotedT 'TZ) res) rest