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
  deriving (Eq, Show)

toSimpleType :: Type a -> Either String SimpleType
toSimpleType IType = Right IST
toSimpleType CType = Right CST
toSimpleType BType = Right BST
toSimpleType (LType IType) = Right ILST
toSimpleType (LType CType) = Right CLST
toSimpleType (LType BType) = Right BLST
toSimpleType _ = Left "Nested lists are not supported"

literalToSimpleType :: Literal -> SimpleType
literalToSimpleType (I i) = IST
literalToSimpleType (C c) = CST
literalToSimpleType (B b) = BST

literalToTypedExpression :: Literal -> Either String TypedExpression
literalToTypedExpression (I i) = Right $ Lit i ::: IType
literalToTypedExpression (C c) = Right $ Lit c ::: CType
literalToTypedExpression (B b) = Right $ Lit b ::: BType
literalToTypedExpression (L l) = do
  case l of
    ILst il -> Right $ Lit il ::: LType IType
    CLst il -> Right $ Lit il ::: LType CType
    BLst il -> Right $ Lit il ::: LType BType

typecheck :: UExpression -> Store -> Either String TypedExpression
typecheck (UVar var) store = do
  case lookup var (runStore store) of
    Just lookedup -> literalToTypedExpression lookedup
    Nothing -> Left "Variable was not found"
typecheck (ULit i) _ = do
  Right $ Lit i ::: getType i
typecheck (UIntCalc o l r) store = do
  (left ::: tl) <- typecheck l store
  (right ::: tr) <- typecheck r store
  case (tl, tr) of
    (IType, IType) -> Right $ IntCalc o left right ::: IType
    _ -> Left "IntCalc needs two Ints"
typecheck (UBoolCalc o l r) store = do
  (left ::: tl) <- typecheck l store
  (right ::: tr) <- typecheck r store
  case (tl, tr) of
    (IType, IType) -> Right $ BoolCalc IType o left right ::: BType
    (BType, BType) -> Right $ BoolCalc BType o left right ::: BType
    (CType, CType) -> Right $ BoolCalc CType o left right ::: BType
    (LType IType, LType IType) -> Right $ BoolCalc (LType IType) o left right ::: BType
    (LType CType, LType CType) -> Right $ BoolCalc (LType CType) o left right ::: BType
    (LType BType, LType BType) -> Right $ BoolCalc (LType BType) o left right ::: BType
    _ -> Left "boolcalc does not work with nested lists"
typecheck (UBinBool o l r) store = do
  (left ::: tl) <- typecheck l store
  (right ::: tr) <- typecheck r store
  case (tl, tr) of
    (BType, BType) -> Right $ BinBool o left right ::: BType
    _ -> Left "BinBool needs to Bools"
typecheck (UUniBool o e) store = do
  (expr ::: t) <- typecheck e store
  case t of
    BType -> Right $ UniBool o expr ::: BType
    _ -> Left "UniBool needs a Bool"
typecheck (URange b e) store = do
  (left ::: tl) <- typecheck b store
  (right ::: tr) <- typecheck e store
  case (tl, tr) of
    (IType, IType) -> Right $ Range left right ::: LType IType
    (CType, CType) -> Right $ Range left right ::: LType CType
    (BType, BType) -> Right $ Range left right ::: LType BType
    _ -> Left "Range only works with Int, Char and Bool"
typecheck (UElem l i) store = do
  (list ::: tl) <- typecheck l store
  (index ::: tr) <- typecheck i store
  case (tl, tr) of
    (LType t, IType) ->
      case t of
        IType -> Right $ Elem list index ::: IType
        CType -> Right $ Elem list index ::: CType
        BType -> Right $ Elem list index ::: BType
        _ -> Left "Elem only works with Int, Char and Bool lists"
    _ -> Left "First argument of Elem needs to be a List and second needs to be an Int"
typecheck (UListDiv r l) store = do
  (right ::: tll) <- typecheck r store
  (left ::: trr) <- typecheck l store
  case (tll, trr) of
    (LType tl, LType tr) ->
      case (tl, tr) of
        (IType, IType) -> Right $ ListDiv right left ::: LType IType
        (CType, CType) -> Right $ ListDiv right left ::: LType CType
        (BType, BType) -> Right $ ListDiv right left ::: LType BType
        _ -> Left "ListDiv only works with Int, Char and Bool"
    _ -> Left "ListDiv needs two lists as arguments"
typecheck (USingleton e) store = do
  (expr ::: t) <- typecheck e store
  case t of
    IType -> Right $ Singleton expr ::: LType IType
    CType -> Right $ Singleton expr ::: LType CType
    BType -> Right $ Singleton expr ::: LType BType
typecheck (UToList l) store = do
  let types =
        ( \case
            Right (_ ::: t) -> toSimpleType t
            Left m -> Left m
        )
          . (`typecheck` store)
          <$> l
  let literals = (`calcSolution` store) <$> l
  if length (nub types) == 1
    then do
      let toTyped x = case x of
            Right IST ->
              Right $
                ToList [Lit i | (I i) <- literals] ::: LType IType
            Right CST ->
              Right $
                ToList [Lit i | (C i) <- literals] ::: LType CType
            Right BST ->
              Right $
                ToList [Lit i | (B i) <- literals] ::: LType BType
            _ -> Left "only list of Int, Char and Bool can be generated, nested lists are not supported"
      toTyped $ head types
    else do
      Left "ONly lists with one type are allowed"

calcSolution :: UExpression -> Store -> Literal
calcSolution exp store = case typecheck exp store of
  Right (e ::: _) -> toLiteral $ eval e store
  Left m -> error m

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
      case l of
        ILst il -> uniform [uExecute (UAssign s (ULit ev)) store | ev <- il]
        CLst il -> uniform [uExecute (UAssign s (ULit ev)) store | ev <- il]
        BLst il -> uniform [uExecute (UAssign s (ULit ev)) store | ev <- il]
        _ -> error "nested lists are not supported yet"
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
  (L l) -> case l of
    ILst il -> uniform $ toLiteral <$> il
    CLst il -> uniform $ toLiteral <$> il
    BLst il -> uniform $ toLiteral <$> il
    _ -> error "nested lists are not supported"
  _ -> error "uniform needs a list"
uObservation (UOChoose p l r) store = choose p (calcSolution l store) (calcSolution r store)
