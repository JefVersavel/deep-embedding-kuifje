{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module State where

import Data.Map hiding (map)
import qualified Data.Map.Strict as HM (elems, mapWithKey)
import Language.Kuifje.PrettyPrint
import Text.PrettyPrint.Boxes
import Type
import Prelude hiding (lookup)

-- could later change it to static typing by adding a type to the string and checking it at compiletime
newtype Store = Store {runStore :: Map String Literal}
  deriving (Eq, Ord)

-- later add the type to the literal to do typechecking at compiletime
data Literal = I Int | C Char | B Bool | L Lst
  deriving (Eq, Ord)

data Lst = Lst [Lst] | ILst [Int] | CLst [Char] | BLst [Bool]
  deriving (Eq, Ord, Show)

instance Boxable Literal where
  toBox (I i) = toBox i
  toBox (B b) = toBox b
  toBox (C c) = text $ show c
  toBox (L l) = toBox l

instance Boxable Lst where
  toBox (Lst l) = toBox l
  toBox (ILst l) = toBox l
  toBox (CLst l) = toBox l
  toBox (BLst l) = toBox l

instance Boxable Store where
  toBox store = tabulate $ HM.elems (HM.mapWithKey lambdaPrint (runStore store))
    where
      lambdaPrint e p = [toBox e, text (show p)]

instance Show Literal where
  show (I i) = show i
  show (C c) = show c
  show (B b) = show b
  show (L l) = show l

showStore :: Store -> Map String String
showStore store = show <$> runStore store

class ToType a where
  toType :: Literal -> a
  toLiteral :: a -> Literal
  getType :: a -> Type a

instance ToType Int where
  toType (I i) = i
  toType _ = error "wrong type"
  toLiteral = I
  getType _ = IType

instance ToType Char where
  toType (C c) = c
  toType _ = error "wrong type"
  toLiteral = C
  getType _ = CType

instance ToType Bool where
  toType (B b) = b
  toType _ = error "wrong type"
  toLiteral = B
  getType _ = BType

instance ToType [Int] where
  toType (L (ILst l)) = l
  toType _ = error "wrong type"
  toLiteral l = L $ ILst l
  getType l = LType IType

instance ToType [Char] where
  toType (L (CLst l)) = l
  toType _ = error "wrong type"
  toLiteral l = L $ CLst l
  getType l = LType CType

instance ToType [Bool] where
  toType (L (BLst l)) = l
  toType _ = error "wrong type"
  toLiteral l = L $ BLst l
  getType l = LType BType

instance ToType [a] => ToType [[a]] where
  toType l = toType l
  toLiteral l = toLiteral l
  getType [] = error "cannot infer type of empty list"
  getType a = LType $ getType $ head a
