module Monomorphic.Terms where

import Monomorphic.Types

data Term = Var Variable 
            | Lambda Variable Type Term
            | Application Term Term
            | ModalVar Variable
            | Box Term
            | LetBox Variable Term Term
            | Pair Term Term
            | First Term
            | Second Term
            | EmptyProduct
            | Zero
            | Succ Term
            | Case Term Term Variable Term
            | Fix Variable Type Term
            | Anno Term Type
    deriving (Show)