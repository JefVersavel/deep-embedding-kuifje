{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Type where

data Type a where
  IType :: Type Int
  CType :: Type Char
  BType :: Type Bool
  LType :: Type a -> Type [a]
