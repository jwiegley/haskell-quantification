{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Example where

import Data.Proxy
import Data.Reflection
import Data.Tagged

foo :: forall a b. a -> b
foo = undefined

foo' :: forall a. a -> forall b. b
foo' = undefined

bar :: forall a b. (forall r s. r -> s) -> a -> b
bar = id

data ExistsEQ = forall a. ExistsEQ a

newtype ExistsRNT = ExistsRNT {
  getExistsRNT ::
    forall r. (forall a. a -> r) -> r
}

data ExistsG where
  ExistsG :: a -> ExistsG

baz :: forall s. Reifies s Int => Tagged s Int -> Int
baz (Tagged n) = n + reflect (Proxy :: Proxy s)
