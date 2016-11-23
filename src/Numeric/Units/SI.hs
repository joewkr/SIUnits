{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Numeric.Units.SI where

import Control.DeepSeq
import GHC.Generics (Generic)
import qualified Prelude as P

import Numeric.Units.SI.Base
import Numeric.Units.SI.Numerals

data SI (a :: Unit) b where
    SI :: !b -> SI a b deriving Generic

deriving instance P.Show b => P.Show (SI a b)

instance NFData b => NFData (SI a b)

instance P.Functor (SI a) where
    fmap f (SI val) = SI (f val)

infixl 7 *, /
infixl 6 +, -

(*) :: P.Num b => SI a1 b -> SI a2 b -> SI (Mult a1 a2) b
(*) (SI l) (SI r) = SI (l P.* r)

(/) :: P.Fractional b => SI a1 b -> SI a2 b -> SI (Div a1 a2) b
(/) (SI l) (SI r) = SI (l P./ r)

(+) :: (P.Num b, NormalForm a1 ~ NormalForm a2) => SI a1 b -> SI a2 b -> SI a1 b
(+) (SI l) (SI r) = SI (l P.+ r)

(-) :: (P.Num b, NormalForm a1 ~ NormalForm a2) => SI a1 b -> SI a2 b -> SI a1 b
(-) (SI l) (SI r) = SI (l P.- r)