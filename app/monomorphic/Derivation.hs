module Monomorphic.Derivation where

import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Context

import Data.List




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

latexPrint :: Derivation -> String
latexPrint (TypeChecks premises (mctx, octx) tm ty) = 
    "\\inferrule*{" ++ (intercalate " " (map latexPrint premises))  ++ "}{" 
        ++ latexPrintMCtx mctx ++ "; " ++ latexPrintOCtx octx ++ " \\vdash " ++ latexPrintTm tm ++ " \\Leftarrow " ++ latexPrintTy ty ++ "}"
latexPrint (TypeSynthesises premises (mctx, octx) tm ty) = 
    "\\inferrule*{" ++ (intercalate " " (map latexPrint premises))  ++ "}{" 
    ++ latexPrintMCtx mctx ++ "; " ++ latexPrintOCtx octx ++ " \\vdash " ++ latexPrintTm tm ++ " \\Rightarrow " ++ latexPrintTy ty ++ "}"
latexPrint (DoesNotCheck premises (mctx, octx) tm ty) = 
    "\\inferrule*{" ++ (intercalate " " (map latexPrint premises))  ++ "}{" 
    ++ latexPrintMCtx mctx ++ "; " ++ latexPrintOCtx octx ++ " \\vdash " ++ latexPrintTm tm ++ " \\not\\Leftarrow " ++ latexPrintTy ty ++ "}"
latexPrint (DoesNotSynthesise premises (mctx, octx) tm) = 
    "\\inferrule*{" ++ (intercalate " " (map latexPrint premises))  ++ "}{" 
    ++ latexPrintMCtx mctx ++ "; " ++ latexPrintOCtx octx ++ " \\vdash " ++ latexPrintTm tm ++ " \\not\\Rightarrow " ++ "}"
latexPrint NoDerivation = ""