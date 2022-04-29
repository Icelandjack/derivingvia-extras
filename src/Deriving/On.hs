{-# Language DataKinds                #-}
{-# Language InstanceSigs             #-}
{-# Language ScopedTypeVariables      #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications         #-}
{-# Language TypeOperators            #-}
{-# Language UndecidableInstances     #-}

module Deriving.On (On(..)) where

import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.Kind     (Type)
import Data.Ord      (comparing)
import GHC.Records   (HasField(..))
import GHC.TypeLits  (Symbol)

-- | With 'DerivingVia': to derive non-structural instances. Specifies
-- what field to base instances on.
--
-- The type @'On' User "userID"@ is compared and evaluated based only
-- on the @"userID"@ record field. This uses 'HasField' from
-- @GHC.Records@ to project the relevant component.
--
-- @
-- {-# Language DataKinds     #-}
-- {-# Language DerivingVia   #-}
-- {-# Language TypeOperators #-}
--
-- import Deriving.On
-- import Data.Hashable
--
-- data User = User
--   { name   :: String
--   , age    :: Int
--   , userID :: Integer
--   }
--   deriving (Eq, Ord, Hashable)
--   via User `On` "userID"
-- @
--
-- @
-- >> alice = User "Alice" 50 0xDEADBEAF
-- >> bob   = User "Bob"   20 0xDEADBEAF
-- >>
-- >> alice == bob
-- True
-- >> alice <= bob
-- True
-- >> hash alice == hash bob
-- True
-- @
type    On :: Type -> Symbol -> Type
newtype a `On` field = On a

instance (HasField field a b, Eq b) => Eq (a `On` field) where
  (==) :: a `On` field -> a `On` field -> Bool
  On a1 == On a2 = ((==) `on` getField @field) a1 a2

instance (HasField field a b, Ord b) => Ord (a `On` field) where
  compare :: a `On` field -> a `On` field -> Ordering
  On a1 `compare` On a2 = comparing (getField @field) a1 a2

instance (HasField field a b, Hashable b) => Hashable (a `On` field) where
  hashWithSalt :: Int -> a `On` field -> Int
  hashWithSalt salt (On a) = hashWithSalt salt (getField @field a)
