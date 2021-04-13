{-# LANGUAGE TypeOperators #-}

module Semantics where

import Language
import Language.Kuifje.Distribution
import Language.Kuifje.Semantics
import State
import Syntax
import TypeCheck
import Prelude hiding (return)

hysemBobby :: Bobby -> (Store ~~> Store)
hysemBobby Skip = return
hysemBobby (Update f p) = huplift (updateStatement f) ==> hysemBobby p
hysemBobby (If c p q r) = conditional (condition c) (hysemBobby p) (hysemBobby q) ==> hysemBobby r
hysemBobby (While c p q) =
  let wh = conditional (condition c) (hysemBobby p ==> wh) (hysemBobby q)
   in wh
hysemBobby (Observe f p) = hobsem (observation' f) ==> hysemBobby p

uHysemBobby :: UBobby -> (Store ~~> Store)
uHysemBobby USkip = return
uHysemBobby (UUpdate f p) = huplift (uUpdateStatement f) ==> uHysemBobby p
uHysemBobby (UIf c p q r) = conditional (uCondition c) (uHysemBobby p) (uHysemBobby q) ==> uHysemBobby r
uHysemBobby (UWhile c p q) =
  let wh = conditional (uCondition c) (uHysemBobby p ==> wh) (uHysemBobby q)
   in wh
uHysemBobby (UObserve f p) = hobsem (uObservation f) ==> uHysemBobby p
