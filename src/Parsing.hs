{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Parsing where

import Arithmetic
import Boolean
import Expression
import State
import Type

data UExpression
  = UVar String
  | UIntLit Int
  | UCharLit Char
  | UBoolLit Bool
  | UIntListLit [Int]
  | UCharListLit [Char]
  | UBoolListLit [Bool]
  | UIntCalc BinOpAri UExpression UExpression
  | BoolCalc BinOpBool UExpression UExpression
  | UniBool UnOpBool UExpression
  | URange UExpression UExpression
  | UElem UExpression UExpression
  | UListDiv UExpression UExpression
  | USingleton UExpression
  | UIntEmpty
  | UCharEmpty
  | UBoolEmpty
  | UToList [UExpression]

typeCheckExp :: UExpression -> Maybe ATExp
typeCheckExp (UVar s) = return $ ATExp (Var s) IType
typeCheckExp (UIntLit i) = return $ ATExp (Lit i) IType
typeCheckExp (UCharLit c) = return $ ATExp (Lit c) CType
typeCheckExp (UIntListLit l) = return $ ATExp (Lit l) (LType IType)
typeCheckExp (UIntCalc o l r) = do
  ATExp left IType <- typeCheckExp l
  ATExp right IType <- typeCheckExp r
  return $ ATExp (IntCalc o left right) IType

data ATExp where
  ATExp :: ToType a => Expression a -> Type a -> ATExp

-- evalATexp e store = do
--   ATExp exp _ <- typeCheckExp e
--   eval exp store
