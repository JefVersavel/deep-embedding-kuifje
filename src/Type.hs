{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Type where

data Type a where
  IType :: Type Int
  CType :: Type Char
  BType :: Type Bool
  LType :: Type a -> Type [a]

instance Eq (Type a) where
  IType == IType = True
  CType == CType = True
  BType == BType = True
  (LType l) == (LType r) = l == r

instance Show (Type a) where
  show IType = "Int"
  show CType = "Char"
  show BType = "Bool"
  show (LType l) = "[" ++ show l ++ "]"
