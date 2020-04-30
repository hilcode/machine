module Hilcode.State
( State(..)
, state
, put
, get
, evalState
, execState
) where

import qualified Control.Applicative

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
    fmap f state = State (fmap f . runState state)

instance Applicative (State s) where
    {-# INLINE pure #-}
    pure a = State (, a)
    liftA2 f (State runStateLft) (State runStateRgt) = State runState
      where
        runState s = case runStateLft s of
            (s', a) -> case runStateRgt s' of
                (s'', b) -> (s'', f a b)

instance Monad (State s) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return = pure
    state >>= f = State runState'
      where
        runState' s = case runState state s of
            (s', a) -> runState (f a) s'

state :: (s -> (s, a)) -> State s a
state = State

put :: s -> State s ()
put s = State (const (s, ()))

get :: State s s
get = State (\s -> (s, s))

evalState :: State s a -> s -> s
evalState state' s = fst (runState state' s)

execState :: State s a -> s -> a
execState state x = snd (runState state x)
