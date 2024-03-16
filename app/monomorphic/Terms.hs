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
            | EmptyPair
            | Zero
            | Succ Term
            | Case Term Term Variable Term
            | Fix Variable Type Term
            | Anno Term Type

instance Show Term where
    show (Var ident) = ident
    show (Lambda arg argType body) = "𝛌" ++ arg ++ ":" ++ show argType ++ "." ++ show body
    show (Application abs to) = "(" ++ show abs ++ ")" ++ show to
    show (ModalVar ident) = ident
    show (Box code) = "box " ++ show code
    show (LetBox ident boxed body) = "let box " ++ ident ++ " = " ++ show boxed ++ " in " ++ show body
    show (Pair left right) = "⟨" ++ show left ++ "," ++ show right ++ "⟩"
    show (First pair) = "fst " ++ show pair
    show (Second pair) = "snd " ++ show pair
    show (EmptyPair) = "⟨⟩"
    show (Zero) = "z"
    show (Succ prev) = "s " ++ show prev
    show (Case depend zeroCase ident succCase) = "(case " ++ show depend ++ " of z => " ++ show zeroCase ++ " | s " ++ ident ++ " => " ++ show succCase ++")"
    show (Fix funcIdent funcTy body) = "fix " ++ funcIdent ++ ":" ++ show funcTy ++ "." ++ show body
    show (Anno term termTy) = "(" ++ show term ++ " : " ++ show termTy ++")"