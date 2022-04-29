{-# Language DataKinds                #-}
{-# Language InstanceSigs             #-}
{-# Language PolyKinds                #-}
{-# Language ScopedTypeVariables      #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeApplications         #-}
{-# Language TypeOperators            #-}
{-# Language UndecidableInstances     #-}

module Deriving.On (On(..)) where

import Data.Function     (on)
import Data.Hashable     (Hashable(..))
import Data.Kind         (Type)
import Data.Ord          (comparing)
import Deriving.On.Class (OnTarget (..))

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
type    On :: forall k. Type -> k -> Type
newtype a `On` field = On a

instance (OnTarget k target a b, Eq b) => Eq (On @k a target) where
  (==) :: a `On` field -> a `On` field -> Bool
  On a1 == On a2 = ((==) `on` getTarget @k @target) a1 a2


instance (OnTarget k target a b, Ord b) => Ord (On @k a target) where
  compare :: a `On` target -> a `On` target -> Ordering
  On a1 `compare` On a2 = comparing (getTarget @k @target) a1 a2

instance (OnTarget k target a b, Hashable b) => Hashable (On @k a target) where
  hashWithSalt :: Int -> a `On` target -> Int
  hashWithSalt salt (On a) = hashWithSalt salt (getTarget @k @target a)
