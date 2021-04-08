{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Type where

data Type a where
  IType :: Type Int
  CType :: Type Char
  BType :: Type Bool
  LType :: Type a -> Type [a]
  LIType :: Type [Int]
  LCType :: Type [Char]
  LBType :: Type [Bool]
  AType :: Type a
