{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeCheck where

import Arithmetic
import Boolean
import Data.List hiding (insert, lookup)
import Data.Map
import Expression
import Language.Haskell.TH hiding (Type)
import Language.Kuifje.Distribution
import State
import Type
import Prelude hiding (lookup, return)

data UExpression
  = UVar String
  | forall a. (ToType a, Show a) => ULit a
  | UIntCalc BinOpAri UExpression UExpression
  | UBoolCalc OpAriBool UExpression UExpression
  | UBinBool BinOpBool UExpression UExpression
  | UUniBool UnOpBool UExpression
  | URange UExpression UExpression
  | UElem UExpression UExpression
  | UListDiv UExpression UExpression
  | USingleton UExpression
  | UToList [UExpression]

data TypedExpression = forall t. (Show t, ToType t) => (Expression t) ::: (Type t)

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

literalToSimpleType :: Literal -> SimpleType
literalToSimpleType (I i) = IST
literalToSimpleType (C c) = CST
literalToSimpleType (B b) = BST

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
            Just IST -> Just $ Lit (toType $ L l) ::: LType IType
            Just CST -> Just $ Lit (toType $ L l) ::: LType CType
            Just BST -> Just $ Lit (toType $ L l) ::: LType BType
            _ -> Nothing
      toTyped $ head types
    else do
      Nothing

typecheck :: UExpression -> Store -> Maybe TypedExpression
typecheck (UVar var) store = do
  lookedup <- lookup var (runStore store)
  literalToTypedExpression lookedup
typecheck (ULit i) _ = do
  Just $ Lit i ::: getType i
typecheck (UIntCalc o l r) store = do
  (left ::: IType) <- typecheck l store
  (right ::: IType) <- typecheck r store
  Just $ IntCalc o left right ::: IType
typecheck (UBoolCalc o l r) store = do
  (left ::: tl) <- typecheck l store
  (right ::: tr) <- typecheck r store
  case (tl, tr) of
    (IType, IType) -> Just $ BoolCalc IType o left right ::: BType
    (BType, BType) -> Just $ BoolCalc BType o left right ::: BType
    (CType, CType) -> Just $ BoolCalc CType o left right ::: BType
    (LType IType, LType IType) -> Just $ BoolCalc (LType IType) o left right ::: BType
    (LType CType, LType CType) -> Just $ BoolCalc (LType CType) o left right ::: BType
    (LType BType, LType BType) -> Just $ BoolCalc (LType BType) o left right ::: BType
    _ -> Nothing
typecheck (UBinBool o l r) store = do
  (left ::: BType) <- typecheck l store
  (right ::: BType) <- typecheck r store
  Just $ BinBool o left right ::: BType
typecheck (UUniBool o e) store = do
  (expr ::: BType) <- typecheck e store
  Just $ UniBool o expr ::: BType
typecheck (URange b e) store = do
  (left ::: tl) <- typecheck b store
  (right ::: tr) <- typecheck e store
  case (tl, tr) of
    (IType, IType) -> Just $ Range left right ::: LType IType
    (CType, CType) -> Just $ Range left right ::: LType CType
    (BType, BType) -> Just $ Range left right ::: LType BType
    _ -> Nothing
typecheck (UElem l i) store = do
  (list ::: LType t) <- typecheck l store
  (index ::: IType) <- typecheck i store
  case t of
    IType -> Just $ Elem list index ::: IType
    CType -> Just $ Elem list index ::: CType
    BType -> Just $ Elem list index ::: BType
    _ -> Nothing
typecheck (UListDiv r l) store = do
  (right ::: LType tl) <- typecheck r store
  (left ::: LType tr) <- typecheck l store
  case (tl, tr) of
    (IType, IType) -> Just $ ListDiv right left ::: LType IType
    (CType, CType) -> Just $ ListDiv right left ::: LType CType
    (BType, BType) -> Just $ ListDiv right left ::: LType BType
    _ -> Nothing
typecheck (USingleton e) store = do
  (expr ::: t) <- typecheck e store
  case t of
    IType -> Just $ Singleton expr ::: LType IType
    CType -> Just $ Singleton expr ::: LType CType
    BType -> Just $ Singleton expr ::: LType BType
typecheck (UToList l) store = do
  let types =
        ( \case
            Just (_ ::: t) -> toSimpleType t
            Nothing -> Nothing
        )
          . (`typecheck` store)
          <$> l
  let literals = (`calcSolution` store) <$> l
  if length (nub types) == 1
    then do
      let toTyped x = case x of
            Just IST ->
              Just $
                ToList [Lit i | (I i) <- literals] ::: LType IType
            Just CST ->
              Just $
                ToList [Lit i | (C i) <- literals] ::: LType CType
            Just BST ->
              Just $
                ToList [Lit i | (B i) <- literals] ::: LType BType
            _ -> Nothing
      toTyped $ head types
    else do
      Nothing

calcSolution :: UExpression -> Store -> Literal
calcSolution exp store = case typecheck exp store of
  Just (e ::: _) -> toLiteral $ eval e store
  Nothing -> error "expression did not compile"

data UStatement = UAssign String UExpression

uExecute :: UStatement -> Store -> Store
uExecute (UAssign var expr) store =
  Store $
    insert var (calcSolution expr store) (runStore store)

data UUpdateLanguage
  = UURet UStatement
  | UUUni [UStatement]
  | UUChoose Prob UStatement UStatement
  | UUUniAssign String UExpression

uUpdateStatement :: UUpdateLanguage -> Store -> Dist Store
uUpdateStatement (UURet s) store = return $ uExecute s store
uUpdateStatement (UUUni l) store = uniform [uExecute s store | s <- l]
uUpdateStatement (UUChoose p l r) store =
  choose p (uExecute l store) (uExecute r store)
uUpdateStatement (UUUniAssign s e) store =
  case calcSolution e store of
    L l ->
      let types = literalToSimpleType <$> l
       in if length (nub types) == 1
            then case head types of
              IST ->
                uniform
                  [uExecute (UAssign s (ULit ev)) store | (I ev) <- l]
              CST ->
                uniform
                  [uExecute (UAssign s (ULit ev)) store | (C ev) <- l]
              BST ->
                uniform
                  [uExecute (UAssign s (ULit ev)) store | (B ev) <- l]
            else error "not good"
    _ -> error "not good"

statTest = uUpdateStatement (UURet (UAssign "x" (ULit 'a'))) (Store empty)

data UConditionLanguage
  = UCRet UExpression
  | UCUni [UExpression]
  | UCChoose Prob UExpression UExpression

literalToBool :: Literal -> Bool
literalToBool (B b) = b
literalToBool _ = error "could not found boolean value"

uCondition :: UConditionLanguage -> Store -> Dist Bool
uCondition (UCRet e) store = return $ literalToBool $ calcSolution e store
uCondition (UCUni l) store =
  uniform $ [literalToBool $ calcSolution e store | e <- l]
uCondition (UCChoose p l r) store =
  choose
    p
    (literalToBool $ calcSolution l store)
    (literalToBool $ calcSolution r store)

data UObserveLanguage
  = UORet UExpression
  | UOUni UExpression
  | UOChoose Prob UExpression UExpression

uObservation :: UObserveLanguage -> Store -> Dist Literal
uObservation (UORet e) store = return $ calcSolution e store
uObservation (UOUni e) store = case calcSolution e store of
  (L l) -> uniform l
  _ -> error "uniform needs a list"
uObservation (UOChoose p l r) store = choose p (calcSolution l store) (calcSolution r store)
