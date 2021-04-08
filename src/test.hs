{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- Type-checing of the lifted terms.
-- The Haskell type-checker is a meta-typechecker

module Test where

import Test2

-- Typed terms
data Term t where
  Lit :: Int -> Term Int
  Inc :: Term Int -> Term Int
  IsZ :: Term Int -> Term Bool
  If :: Term Bool -> Term a -> Term a -> Term a
  Pair :: Term a -> Term b -> Term (a, b)
  Fst :: Term (a, b) -> Term a
  Snd :: Term (a, b) -> Term b

-- Typed evaluator
eval :: Term t -> t
eval (Lit i) = i
eval (Inc t) = eval t + 1
eval (IsZ t) = eval t == 0
eval (If b t e) = if eval b then eval t else eval e
eval (Pair a b) = (eval a, eval b)
eval (Fst t) = fst (eval t)
eval (Snd t) = snd (eval t)

-- Figure out the type of the expression e
-- The type-checking rules are written in the natural notation

class TypeCheck e t | e -> t where
  typecheck :: e -> Term t

instance TypeCheck FLit Int where
  typecheck (FLit x) = Lit x

instance TypeCheck e Int => TypeCheck (FInc e) Int where
  typecheck (FInc e) = Inc (typecheck e)

instance TypeCheck e Int => TypeCheck (FIsZ e) Bool where
  typecheck (FIsZ e) = IsZ (typecheck e)

instance
  (TypeCheck e1 Bool, TypeCheck e2 t, TypeCheck e3 t) =>
  TypeCheck (FIf e1 e2 e3) t
  where
  typecheck (FIf e1 e2 e3) = If (typecheck e1) (typecheck e2) (typecheck e3)

instance
  (TypeCheck e1 t1, TypeCheck e2 t2) =>
  TypeCheck (FPair e1 e2) (t1, t2)
  where
  typecheck (FPair e1 e2) = Pair (typecheck e1) (typecheck e2)

instance TypeCheck e (t1, t2) => TypeCheck (FFst e) t1 where
  typecheck (FFst e) = Fst (typecheck e)

instance TypeCheck e (t1, t2) => TypeCheck (FSnd e) t2 where
  typecheck (FSnd e) = Snd (typecheck e)

t0' = $(parse . read $ "EInc (EInc (ELit 1))")

t1' = $(parse . read $ e1)

t2' = $(parse . read $ e2)

{-
*G> :t t0'
t0' :: FInc (FInc FLit)
*G> :t t1'
t1' :: FIf FLit FLit FLit
*G> :t t2'
t2' :: FIf (FIsZ FLit) (FInc FLit) (FFst (FPair FLit FLit))
-}

tt0 = typecheck t0'

-- Causes the typechecking error: cannot match Int against Bool
-- tt1 = typecheck t1'

tt2 = typecheck t2'

{-
*G> :t tt2
tt2 :: Term Int
-}

ttt0 = eval tt0 -- 3

ttt2 = eval tt2 -- 2
