{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module TypeCheck where

import Arithmetic
import Boolean
import Data.List hiding (lookup)
import Data.Map
import Expression
import State
import Type
import Prelude hiding (lookup)

data UExpression
  = UVar String
  | UIntLit Int
  | UCharLit Char
  | UBoolLit Bool
  | UIntListLit [Int]
  | UCharListLit [Char]
  | UBoolListLit [Bool]
  | UIntCalc BinOpAri UExpression UExpression
  | UBoolCalcInt OpAriBool UExpression UExpression
  | UBoolCalcChar OpAriBool UExpression UExpression
  | UBoolCalcIntList OpAriBool UExpression UExpression
  | UBinBool BinOpBool UExpression UExpression
  | UUniBool UnOpBool UExpression
  | URange UExpression UExpression
  | UElem UExpression UExpression
  | UListDiv UExpression UExpression
  | USingleton UExpression
  | UIntEmpty
  | UCharEmpty
  | UBoolEmpty
  | UToList [UExpression]

data TypedExpression = forall t. (Expression t) ::: (Type t)

instance Show TypedExpression where
  show (e ::: t) = show t ++ " " ++ show e

data SimpleType = IST | CST | BST | ILST | CLST | BLST
  deriving (Eq)

toSimpleType :: Type a -> Maybe SimpleType
toSimpleType IType = Just IST
toSimpleType CType = Just CST
toSimpleType BType = Just BST
toSimpleType (LType IType) = Just ILST
toSimpleType (LType CType) = Just CLST
toSimpleType (LType BType) = Just BLST
toSimpleType _ = Nothing

literalToTypedExpression :: Literal -> Maybe TypedExpression
literalToTypedExpression (I i) = Just $ Lit i ::: IType
literalToTypedExpression (C c) = Just $ Lit c ::: CType
literalToTypedExpression (B b) = Just $ Lit b ::: BType
literalToTypedExpression (L l) = do
  let types =
        ( \case
            Just (_ ::: t) -> toSimpleType t
            Nothing -> Nothing
        )
          <$> [literalToTypedExpression lit | lit <- l]
  if length (nub types) == 1
    then do
      let toTyped x = case x of
            Just IST -> return $ Lit (toType $ L l) ::: LType IType
            Just CST -> return $ Lit (toType $ L l) ::: LType CType
            Just BST -> return $ Lit (toType $ L l) ::: LType BType
            _ -> Nothing
      toTyped $ head types
    else do
      Nothing

typecheck :: UExpression -> Store -> Maybe TypedExpression
typecheck (UVar var) store = do
  lookedup <- lookup var (runStore store)
  literalToTypedExpression lookedup
typecheck (UIntLit i) _ = Just $ Lit i ::: IType
typecheck (UCharLit i) _ = Just $ Lit i ::: CType
typecheck (UBoolLit i) _ = Just $ Lit i ::: BType
typecheck (UIntListLit i) _ = Just $ Lit i ::: LType IType
typecheck (UCharListLit i) _ = Just $ Lit i ::: LType CType
typecheck (UBoolListLit i) _ = Just $ Lit i ::: LType BType
typecheck (UIntCalc o l r) store = do
  (left ::: IType) <- typecheck l store
  (right ::: IType) <- typecheck r store
  return $ IntCalc o left right ::: IType
typecheck (UBoolCalcInt o l r) store = do
  (left ::: IType) <- typecheck l store
  (right ::: IType) <- typecheck r store
  return $ BoolCalc IType o left right ::: BType
typecheck (UBoolCalcChar o l r) store = do
  (left ::: CType) <- typecheck l store
  (right ::: CType) <- typecheck r store
  return $ BoolCalcChar o left right ::: BType
typecheck (UBoolCalcIntList o l r) store = do
  (left ::: LType IType) <- typecheck l store
  (right ::: LType IType) <- typecheck r store
  return $ BoolCalcIntList o left right ::: BType
typecheck (UBinBool o l r) store = do
  (left ::: BType) <- typecheck l store
  (right ::: BType) <- typecheck r store
  return $ BinBool o left right ::: BType
