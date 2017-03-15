{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Example where

import Control.Applicative hiding (empty)
import Control.Monad.Trans.State
import Control.Monad.Writer
import Data.Dynamic
import Data.IntMap
import Data.Reflection
import Data.Tagged

foo :: forall a b. a -> b
foo = undefined

foo' :: forall a. a -> forall b. b
foo' = undefined

bar :: forall a b. (forall r s. r -> s) -> a -> b
bar = id

data Exists = forall a. Exists a

newtype ExistsRNT = ExistsRNT {
  getExistsRNT ::
    forall r. (forall a. a -> r) -> r
}

data ExistsG where
  ExistsG :: a -> ExistsG

type Lens' s a =
  forall f. Functor f
    => (a -> f a) -> s -> f s

_fst :: Lens' (Int, Int) Int
_fst f (x, y) = (,y) <$> f x

_snd :: Lens' (Int, Int) Int
_snd f (x, y) = (x,) <$> f y

hmm :: Lens' (Int, Int) Int -> (Int, Int) -> Int
hmm l p = getConst $ l Const p

hmm' :: (forall f. Functor f
           => (Int -> f Int)
           -> (Int, Int)
           -> f (Int, Int))
     -> (Int, Int)
     -> Int
hmm' l p = getConst $ l Const p

baz :: forall s. Reifies s Int => Tagged s Int -> Int
baz (Tagged n) = n + reflect (Proxy :: Proxy s)

-- type T = (Int, forall a. a -> Int)

-- type TLens = (Int, Lens' (Int, Int) Int)

newtype Wrapped r =
    Wrapped { getWrapped :: forall a. a -> r }

type T = (Int, Wrapped Int)

data Machine i l o = forall s. Machine
    { monitorState :: s
    , monitorFunc  ::
        i -> StateT s (Writer [l]) o
    }

data Typed = forall a. Typeable a => Typed a

newtype ST s a = ST {
  getST :: State (IntMap Typed) a
} deriving (Functor, Applicative, Monad)

newtype STRef s a = STRef {
  getSTRef :: Int
}

runST :: (forall s. ST s a) -> a
runST (ST s) = evalState s empty

newSTRef :: Typeable a => a -> ST s (STRef s a)
newSTRef a = ST $ do
    m <- get
    let n = size m
    put $ insert n (Typed a) m
    return $ STRef n

readSTRef :: Typeable a => STRef s a -> ST s a
readSTRef (STRef n) = ST $ do
    m <- get
    case m ! n of
        Typed x -> case cast x of
            Nothing -> error "readSTRef: wrong type"
            Just y  -> return y

writeSTRef :: Typeable a => STRef s a -> a -> ST s ()
writeSTRef (STRef n) a = ST $
    modify (update (const (Just (Typed a))) n)

test :: Int
test =
    -- We pass a universally quantified 'action'; in order to use it, 'runST'
    -- must pick a type for 's', but which type will it used? It "skolemizes"
    -- the action by fabricating a new type and fixing the action at that
    -- type. Since the choice of 's' is not returned by 'runST', all knowledge
    -- of it is confined to the scope of the 'runST' evaluation, meaning: Any
    -- 'STRef's created during 'runST' may not be referenced after.
    runST action
  where
    action :: forall s. ST s Int
    action = do
        x <- newSTRef 100
        writeSTRef x 500
        readSTRef x

-- broken :: Int
-- broken =
--     let v :: STRef _ Int =
--             runST $ newSTRef 100
--     in runST $ do
--         writeSTRef v 500
--         readSTRef v

-- Not even ScopedTypeVariables can help us here.
