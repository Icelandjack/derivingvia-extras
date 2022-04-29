{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
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
import Data.Kind (Type)

class OnTarget k (target :: k) a b | k target a -> b where
  getTarget :: a -> b

instance HasField field a b => OnTarget Symbol field a b where
  getTarget = getField @field

instance (OnTarget Type t a b, OnTarget Type u a c) => OnTarget Type (t, u) a (b, c) where
  getTarget a = (getTarget @_ @t a, getTarget @_ @u a)

instance (OnTarget k t a b, OnTarget l u a c) => OnTarget (k, l) '(t, u) a (b, c) where
  getTarget a = (getTarget @k @t a, getTarget @l @u a)
