module State where

import Data.Map hiding (map)

-- could later change it to static typing by adding a type to the string and checking it at compiletime
type Store = Map String Literal

-- later add the type to the literal to do typechecking at compiletime
data Literal = I Int | C Char | B Bool | L [Literal]
  deriving (Eq, Ord)

instance Show Literal where
  show (I i) = show i
  show (C c) = show c
  show (B b) = show b
  show (L l) = show l

showStore :: Store -> Map String String
showStore store = show <$> store

class ToLiteral a where
  toLiteral :: a -> Literal
  fromLiteral :: Literal -> a

instance ToLiteral Int where
  toLiteral = I
  fromLiteral (I i) = i
  fromLiteral _ = error "wrong type"

instance ToLiteral Char where
  toLiteral = C
  fromLiteral (C i) = i
  fromLiteral _ = error "wrong type"

instance ToLiteral Bool where
  toLiteral = B
  fromLiteral (B i) = i
  fromLiteral _ = error "wrong type"

instance ToLiteral a => ToLiteral [a] where
  toLiteral l = L $ map toLiteral l
  fromLiteral (L l) = [fromLiteral i | i <- l]
  fromLiteral _ = error "wrong type"
