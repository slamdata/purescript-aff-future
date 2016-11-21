module Control.Monad.Aff.Future
  ( Future
  , defer
  , defer'
  , wait
  , promise
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff.AVar (AffAVar, AVAR, makeVar', makeVar, takeVar, putVar, modifyVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)

import Data.Either (Either(..))
import Data.Foldable (foldl, sequence_)
import Data.HeytingAlgebra (ff, tt, implies)
import Data.List ((:))
import Data.Monoid (class Monoid, mempty)

newtype Future a = Future (∀ eff. AffAVar eff a)

-- | Forks an asynchronous computation, returning a Future. Future results are
-- | shared among consumers (one-to-many resolution). Futures are lazy, such
-- | that the computation will not initiate until a consumer requests it.
defer
  ∷ ∀ m eff e a
  . ( MonadAff (avar ∷ AVAR | eff) m
    , MonadFork e m
    )
  ⇒ m a
  → m (Future a)
defer run = do
  cell ← liftAff (makeVar' (Left false))
  consumers ← liftAff (makeVar' mempty)
  force ← liftAff makeVar

  fork do
    liftAff (takeVar force)
    res ← run
    liftAff do
      fns ← takeVar consumers
      modifyVar (const (Right res)) cell
      putVar consumers mempty
      sequence_ (foldl (\xs a → putVar a res : xs) mempty fns)

  pure $ Future do
    res ← takeVar cell
    case res of
      Right a → do
        putVar cell res
        pure a
      Left b → do
        putVar cell (Left true)
        res' ← makeVar
        modifyVar (res' : _) consumers
        unless b (putVar force unit)
        takeVar res'

-- | Forks an asynchronous computation, initiating it immediately.
defer'
  ∷ ∀ m eff e a
  . ( MonadAff (avar ∷ AVAR | eff) m
    , MonadFork e m
    )
  ⇒ m a
  → m (Future a)
defer' run = do
  res ← defer run
  fork (wait res)
  pure res

-- | Blocks until the Future completes, returning the result. If the Future has
-- | already completed, then it will yield immediately.
wait
  ∷ ∀ m eff a
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Future a
  → m a
wait (Future run) = liftAff run

-- | Returns a Future paired with an effect to resolve it.
promise
  ∷ ∀ m eff a
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ m { future ∷ Future a, resolve ∷ a → m Unit }
promise = liftAff do
  avar ← makeVar
  future ← defer (takeVar avar)
  pure { future, resolve: liftAff <<< putVar avar }

instance functorFuture ∷ Functor Future where
  map f (Future run) = Future (f <$> run)

instance applyFuture ∷ Apply Future where
  apply = ap

instance applicativeFuture ∷ Applicative Future where
  pure = Future <<< pure

instance bindFuture ∷ Bind Future where
  bind (Future run) k = Future (run >>= k >>> wait)

instance monadFuture ∷ Monad Future

instance semigroupFuture ∷ (Semigroup a) ⇒ Semigroup (Future a) where
  append = lift2 append

instance monoidFuture ∷ (Monoid a) ⇒ Monoid (Future a) where
  mempty = pure mempty

instance monadRecFuture ∷ MonadRec Future where
  tailRecM k a = Future (tailRecM (wait <<< k) a)

instance heytingAlgebraFuture ∷ HeytingAlgebra a ⇒ HeytingAlgebra (Future a) where
  not = map not
  disj = lift2 disj
  conj = lift2 conj
  implies = lift2 implies
  tt = pure tt
  ff = pure ff

instance booleanAlgebraFuture ∷ BooleanAlgebra a ⇒ BooleanAlgebra (Future a)

instance semiringFuture ∷ Semiring a ⇒ Semiring (Future a) where
  one = pure one
  mul = lift2 mul
  zero = pure zero
  add = lift2 add

instance ringFuture ∷ Ring a ⇒ Ring (Future a) where
  sub = lift2 sub

instance commutativeRingFuture ∷ CommutativeRing a ⇒ CommutativeRing (Future a)
