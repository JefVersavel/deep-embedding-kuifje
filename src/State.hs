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

instance ToLiteral Int where
  toLiteral = I

instance ToLiteral Char where
  toLiteral = C

instance ToLiteral Bool where
  toLiteral = B

instance ToLiteral a => ToLiteral [a] where
  toLiteral l = L $ map toLiteral l
