module State where

import Control.Lens hiding (Profunctor)
import Data.ByteString
import Data.Map
import Data.Map.Strict (elems)
import Data.Maybe
import Data.Serialize
import Language.Kuifje.Distribution
import Language.Kuifje.PrettyPrint ()
import Language.Kuifje.Semantics
import Language.Kuifje.Syntax
import Prelude hiding (exp, fmap, return)

-- could later change it to static typing by adding a type to the string and checking it at compiletime
type Store = Map String Literal

type TypeStore = Map String Type

data Value = By ByteString | Li [Value]

data Type = I | B | C | L Type

-- later add the type to the literal to do typechecking at compiletime
data Literal = Literal Type Value

instance Show Literal where
  show (Literal I (By b)) = case (decode b :: Either String Int) of
    Left er -> er
    Right i -> show i
  show (Literal B (By b)) = case (decode b :: Either String Bool) of
    Left er -> er
    Right b -> show b
  show (Literal C (By b)) = case (decode b :: Either String Char) of
    Left er -> er
    Right c -> show c
  show (Literal (L t) (Li values)) = show $ [show (Literal t v) | v <- values]

showStore :: Store -> Map String String
showStore store = show <$> store
