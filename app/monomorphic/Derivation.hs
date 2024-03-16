module Monomorphic.Derivation where

import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Context




data Derivation = TypeChecks [Derivation] (ModalContext, OrdinaryContext) Term Type | TypeSynthesises [Derivation] (ModalContext, OrdinaryContext) Term Type 
                | DoesNotCheck [Derivation] (ModalContext, OrdinaryContext) Term Type | DoesNotSynthesise [Derivation] (ModalContext, OrdinaryContext) Term
                | NoDerivation

prettyPrint :: Int -> Derivation -> String
prettyPrint level (TypeChecks premises (mctx, octx) tm ty) = 
    foldl (++) "" (map (\premise -> prettyPrint (level + 1) premise) premises) 
    ++ prefix ++ " = " ++ show mctx ++ "; " ++ show octx ++ " ├ " ++ show tm ++ " <= " ++ show ty ++ "\n"
    where prefix = foldl (++) "" (take level (repeat " |"))
prettyPrint level (TypeSynthesises premises (mctx, octx) tm ty) = 
    foldl (++) "" (map (\premise -> prettyPrint (level + 1) premise) premises)
    ++ prefix ++ " = " ++ show mctx ++ "; " ++ show octx ++ " ├ " ++ show tm ++ " => " ++ show ty ++ "\n"
    where prefix = foldl (++) "" (take level (repeat " |"))
prettyPrint level (DoesNotCheck premises (mctx, octx) tm ty) = 
    foldl (++) "" (map (\premise -> prettyPrint (level + 1) premise) premises)
    ++ prefix ++ " = " ++ show mctx ++ "; " ++ show octx ++ " ├ " ++ show tm ++ " <\\= " ++ show ty ++ "\n"
    where prefix = foldl (++) "" (take level (repeat " |"))
prettyPrint level (DoesNotSynthesise premises (mctx, octx) tm) = 
    foldl (++) "" (map (\premise -> prettyPrint (level + 1) premise) premises)
    ++ prefix ++ " = " ++ show mctx ++ "; " ++ show octx ++ " ├ " ++ show tm ++ " =\\> " ++ "\n"
    where prefix = foldl (++) "" (take level (repeat " |"))
prettyPrint level NoDerivation = 
    prefix ++ " = " ++ "No Derivation" ++ "\n"
    where prefix = foldl (++) "" (take level (repeat " |"))

instance Show Derivation where
    show deriv = prettyPrint 0 deriv