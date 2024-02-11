module Data.Type (Type(..), Scheme(..)) where

import           Data.Expr (Name)
import qualified Data.Set  as S (Set)

data Type = TVar Name
  | TInt | TBool
  | TArrow Type Type
  | TList Type
  deriving Eq

data Scheme = Forall (S.Set Name) Type

instance Show Type where
  show (TVar x) = x
  show TInt           = "int"
  show TBool          = "bool"
  show (TArrow t1 t2)
    | TArrow _ _ <- t1 = "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
    | otherwise = show t1 ++ " -> " ++ show t2
  show (TList t) = "list " ++ show t

instance Show Scheme where
  show (Forall _ t) = show t
