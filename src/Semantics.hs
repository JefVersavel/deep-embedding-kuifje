{-# LANGUAGE TypeOperators #-}

module Semantics where

import Language
import Language.Kuifje.Distribution
import Language.Kuifje.Semantics
import State
import Syntax
import Prelude hiding (return)

hysemBobby :: Bobby -> (Store ~~> Store)
hysemBobby Skip = return
hysemBobby (Update f p) = huplift (updateStatement f) ==> hysemBobby p
hysemBobby (If c p q r) = conditional (condition c) (hysemBobby p) (hysemBobby q) ==> hysemBobby r
hysemBobby (While c p q) =
  let wh = conditional (condition c) (hysemBobby p ==> wh) (hysemBobby q)
   in wh
hysemBobby (Observe f p) = hobsem (observation' f) ==> hysemBobby p
