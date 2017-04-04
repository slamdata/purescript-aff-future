{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Main where

import Prelude
import Control.Monad.Aff (Aff, forkAff, launchAff, delay)
import Control.Monad.Aff.AVar (AVAR, makeVar', modifyVar, peekVar)
import Control.Monad.Aff.Future (defer, defer', wait, promise)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))

type Effects eff =
  ( console ∷ CONSOLE
  , avar ∷ AVAR
  | eff
  )

assert ∷ ∀ eff. Boolean → Aff eff Unit
assert a = unless a (throwError (error "Assertion failed"))

later' ∷ ∀ eff a. Int → Aff eff a → Aff eff a
later' n aff = delay (Milliseconds $ toNumber n) *> aff

test_lazy_defer ∷ ∀ eff. Aff (Effects eff) Unit
test_lazy_defer = void do
  avar ← makeVar' 0
  slowInt ← defer do
    modifyVar (_ + 2) avar
    peekVar avar
  
  void $ forkAff do
    res ← later' 100 (wait slowInt)
    modifyVar (_ + res) avar

  assert <<< eq 0 =<< peekVar avar
  assert <<< eq 4 =<< later' 200 (peekVar avar)
  log "OK"

test_strict_defer ∷ ∀ eff. Aff (Effects eff) Unit
test_strict_defer = void do
  avar ← makeVar' 0
  slowInt ← defer' do
    modifyVar (_ + 2) avar
    peekVar avar
  
  void $ forkAff do
    res ← later' 100 (wait slowInt)
    modifyVar (_ + res) avar

  assert <<< eq 2 =<< peekVar avar
  assert <<< eq 4 =<< later' 200 (peekVar avar)
  log "OK"

test_defer_sharing ∷ ∀ eff. Aff (Effects eff) Unit
test_defer_sharing = void do
  avar ← makeVar' 0
  slowInt ← defer do
    later' 100 (modifyVar (_ + 2) avar)
    peekVar avar

  void $ forkAff do
    res ← wait slowInt
    modifyVar (_ + res) avar

  void $ forkAff do
    res ← wait slowInt
    modifyVar (_ + res) avar

  assert <<< eq 6 =<< later' 200 (peekVar avar)
  log "OK"

test_promise ∷ ∀ eff. Aff (Effects eff) Unit
test_promise = void do
  avar ← makeVar' 0
  { future, resolve } ← promise

  void $ forkAff do
    res ← wait future
    modifyVar (_ + res) avar

  later' 100 (resolve 42)
  assert <<< eq 42 =<< peekVar avar
  log "OK"

test_tailRecM ∷ ∀ eff. Aff (Effects eff) Unit
test_tailRecM = do
  res ← wait $ pure 1000000 >>= tailRecM go
  assert (res == 0)
  log "OK"
  where
  go n | n == 0 = pure (Done 0)
  go n          = pure (Loop (n - 1))

main ∷ Eff (Effects (exception ∷ EXCEPTION)) Unit
main = void $ launchAff do
  log "Testing lazy defer..."
  test_lazy_defer

  log "Testing strict defer..."
  test_strict_defer

  log "Testing defer/wait sharing..."
  test_defer_sharing

  log "Testing promise..."
  test_promise

  log "Testing tailRecM..."
  test_tailRecM
