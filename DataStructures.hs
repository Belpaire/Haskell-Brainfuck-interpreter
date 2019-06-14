module DataStructures
( Expr(..)
  ) where
  
import           Data.Word

data Expr
  = IncrementP Expr
  | DecrementP Expr
  | IncrementV Expr
  | DecrementV Expr
  | OutputV Expr
  | InputV Int
           Expr
  | BracketCommand Expr
                   Expr
  | EndCommand deriving(Show,Eq)
