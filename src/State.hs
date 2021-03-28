module State where

import Data.Map hiding (map)
import qualified Data.Map.Strict as HM (elems, mapWithKey)
import Language.Kuifje.PrettyPrint
import Text.PrettyPrint.Boxes

-- could later change it to static typing by adding a type to the string and checking it at compiletime
newtype Store = Store {runStore :: Map String Literal}
  deriving (Eq, Ord)

-- later add the type to the literal to do typechecking at compiletime
data Literal = I Int | C Char | B Bool | L [Literal]
  deriving (Eq, Ord)

instance Boxable Literal where
  toBox (I i) = toBox i
  toBox (B b) = toBox b
  toBox (C c) = text $ show c
  toBox (L l) = toBox l

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

class ToLiteral a where
  toLiteral :: a -> Literal

class ToType a where
  toType :: Literal -> a

instance ToType Int where
  toType (I i) = i
  toType _ = error "wrong type"

instance ToType Char where
  toType (C c) = c
  toType _ = error "wrong type"

instance ToType Bool where
  toType (B b) = b
  toType _ = error "wrong type"

instance ToType a => ToType [a] where
  toType (L l) = map toType l
  toType _ = error "wrong type"

instance ToLiteral Int where
  toLiteral = I

instance ToLiteral Char where
  toLiteral = C

instance ToLiteral Bool where
  toLiteral = B

instance ToLiteral a => ToLiteral [a] where
  toLiteral l = L $ map toLiteral l
