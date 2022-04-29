{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

module Deriving.On.Class where

import GHC.Records (HasField (getField))
import GHC.TypeLits (Symbol)

class OnTarget k (target :: k) a b | k target a -> b where
  getTarget :: a -> b

instance HasField field a b => OnTarget Symbol field a b where
  getTarget = getField @field
