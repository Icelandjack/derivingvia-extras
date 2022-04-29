{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}

module Deriving.On.Nth where

import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.Kind     (Constraint, Type)
import Data.Ord      (comparing)
import Deriving.On   (On (..))
import qualified GHC.Generics as G
import GHC.TypeLits  (CmpNat, ErrorMessage (..), KnownNat, Nat, TypeError, type (+), type (-), type (<=?))

-- | With 'DerivingVia': to derive non-structural instances with @'On'.
-- @'Nth' specifies what field to base instances on based on its position in
-- a product type.
--
-- Does not support types with multiple constructors.
type Nth :: Nat -> Type
data Nth n

instance (HasNth n a b, Eq b) => Eq (a `On` Nth n) where
  (==) :: a `On` Nth n -> a `On` Nth n -> Bool
  On a1 == On a2 = ((==) `on` getNth @n) a1 a2

instance (HasNth n a b, Ord b) => Ord (a `On` Nth n) where
  compare :: a `On` field -> a `On` field -> Ordering
  On a1 `compare` On a2 = comparing (getNth @n) a1 a2

instance (HasNth n a b, Hashable b) => Hashable (a `On` Nth n) where
  hashWithSalt :: Int -> a `On` field -> Int
  hashWithSalt salt (On a) = hashWithSalt salt (getNth @n a)

-- High-level constraint synonym + helper

type HasNth n a v = (G.Generic a, GHasNth n (G.Rep a) a, v ~ Value n (G.Rep a))

getNth :: forall n a v. HasNth n a v => a -> v
getNth = gGetNth @n @_ @a . G.from

-- Generically get a value from a product type

class (KnownNat n) => GHasNth n t originalTypeForErrorReporting where
  type Value n t :: Type
  gGetNth :: t x -> Value n t

instance (KnownNat n, GHasNth n constructors original) => GHasNth n (G.D1 meta constructors) original where
  type Value n (G.D1 meta constructors) = Value n constructors
  gGetNth (G.M1 c) = gGetNth @n @_ @original c

instance
  ( KnownNat n,
    v ~ (),
    TypeError ('Text "Nth does not work on sum types like" ':$$: 'ShowType original)
  ) =>
  GHasNth n (l G.:+: r) original
  where
  type Value n (l G.:+: r) = TypeError ('Text "Nth does not work on sum types")
  gGetNth = error "Nth does not work on sum types"

instance
  ( KnownNat n,
    CmpNat n (SelectorSize selectors) ~ 'LT,
    If @Constraint
      (SelectorSize selectors <=? n)
      ( TypeError
          ( 'Text "Specified index Nth " ':<>: 'ShowType n ':<>: 'Text " is too large for type"
              ':$$: 'ShowType original
              ':$$: 'ShowType (SelectorSize selectors)
          )
      )
      (),
    (GHasNth n selectors original)
  ) =>
  GHasNth n (G.C1 meta selectors) original
  where
  type Value n (G.C1 meta selectors) = Value n selectors
  gGetNth (G.M1 c) = gGetNth @n @_ @original c

instance
  (n ~ 0) =>
  GHasNth n (G.S1 meta (G.K1 metaK v)) original
  where
  type Value n (G.S1 meta (G.K1 metaK v)) = v
  gGetNth (G.M1 (G.K1 v)) = v

instance
  ( KnownNat n,
    GetNowOrLater (PositiveNat n) selectorL selectorR original
  ) =>
  GHasNth n (selectorL G.:*: selectorR) original
  where
  type Value n (selectorL G.:*: selectorR) = SValue (PositiveNat n) selectorL selectorR
  gGetNth (l G.:*: r) = getNowOrLater @_ @(PositiveNat n) @_ @_ @original l r

type GetNowOrLater :: Maybe Nat -> (k -> Type) -> (k -> Type) -> Type -> Constraint
class GetNowOrLater positiveN now later originalForErrorReporting where
  type SValue positiveN now later :: Type
  getNowOrLater :: now x -> later x -> SValue positiveN now later

instance GetNowOrLater 'Nothing (G.S1 meta (G.K1 metaK v)) later originalForErrorReporting where
  type SValue 'Nothing (G.S1 meta (G.K1 metaK v)) later = v
  getNowOrLater (G.M1 (G.K1 x)) _ = x

instance
  (GHasNth n later originalForErrorReporting) =>
  GetNowOrLater ('Just n) (G.S1 meta t) later originalForErrorReporting
  where
  type SValue ('Just n) (G.S1 meta t) later = Value n later
  getNowOrLater _ later = gGetNth @n @_ @originalForErrorReporting later

-- Helper type families

type family PositiveNat n :: Maybe Nat where
  PositiveNat 0 = 'Nothing
  PositiveNat n = 'Just (n - 1)

type family SelectorSize t :: Nat where
  SelectorSize (G.S1 _ _) = 1
  SelectorSize (l G.:*: r) = SelectorSize l + SelectorSize r

type If :: forall k. Bool -> k -> k -> k
type family If c t e where
  If 'True t _ = t
  If 'False _ e = e
