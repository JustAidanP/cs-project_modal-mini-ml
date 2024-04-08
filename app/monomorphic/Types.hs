module Monomorphic.Types where

type Variable = String
data Type = Natural | Abstraction Type Type | Product Type Type | Unit | Boxed Type
    deriving (Eq)

instance Show Type where
    show Natural = "nat"
    show (Abstraction from to) = "(" ++ show from ++ " -> " ++ show to ++ ")"
    show (Product left right) = "(" ++ show left ++ " ⨉ " ++ show right ++ ")"
    show Unit = "1"
    show (Boxed ty) = "☐" ++ show ty

latexPrintTy :: Type -> String
latexPrintTy Natural = "\\textbf{nat}"
latexPrintTy (Abstraction from to) = "(" ++ latexPrintTy from ++ " \\rightarrow\\ " ++ latexPrintTy to ++ ")"
latexPrintTy (Product left right) = "(" ++ latexPrintTy left ++ " \\times\\ " ++ latexPrintTy right ++ ")"
latexPrintTy Unit = "1"
latexPrintTy (Boxed ty) = "\\Box\\ " ++ latexPrintTy ty