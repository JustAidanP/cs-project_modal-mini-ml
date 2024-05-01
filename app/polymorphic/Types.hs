module Polymorphic.Types where

import Data.List

-- Some predefined variable idents
typeVariableAlpha = "\alpha"

type Variable = String
data MonoType = Natural | Abstraction MonoType MonoType | Product MonoType MonoType | Unit | Boxed MonoType | TypeVariable Variable
    deriving (Eq)

freeVariablesMonoType :: MonoType -> [Variable]
freeVariablesMonoType Natural = []
freeVariablesMonoType (Abstraction from to) = nub ((freeVariablesMonoType from) ++ (freeVariablesMonoType to))
freeVariablesMonoType (Product left right) = nub ((freeVariablesMonoType left) ++ (freeVariablesMonoType right))
freeVariablesMonoType Unit = []
freeVariablesMonoType (Boxed ty) = freeVariablesMonoType ty
freeVariablesMonoType (TypeVariable var) = [var]

substituteInMonoType :: MonoType -> Variable -> MonoType -> MonoType
substituteInMonoType (Abstraction from to) var ty = (Abstraction (substituteInMonoType from var ty) (substituteInMonoType to var ty))
substituteInMonoType (Product left right) var ty = (Product (substituteInMonoType left var ty) (substituteInMonoType right var ty))
substituteInMonoType (Boxed boxedTy) var ty = (Boxed (substituteInMonoType boxedTy var ty))
substituteInMonoType (TypeVariable monoTyVar) var ty | monoTyVar == var = ty
substituteInMonoType subject var ty = subject

instance Show MonoType where
    show Natural = "nat"
    show (Abstraction from to) = "(" ++ show from ++ " -> " ++ show to ++ ")"
    show (Product left right) = "(" ++ show left ++ " ⨉ " ++ show right ++ ")"
    show Unit = "1"
    show (Boxed ty) = "☐" ++ show ty
    show (TypeVariable var) | var == "\alpha" = "⍺"
    show (TypeVariable var) = show var

latexPrintMonoTy :: MonoType -> String
latexPrintMonoTy Natural = "\\textbf{nat}"
latexPrintMonoTy (Abstraction from to) = "(" ++ latexPrintMonoTy from ++ " \\rightarrow\\ " ++ latexPrintMonoTy to ++ ")"
latexPrintMonoTy (Product left right) = "(" ++ latexPrintMonoTy left ++ " \\times\\ " ++ latexPrintMonoTy right ++ ")"
latexPrintMonoTy Unit = "1"
latexPrintMonoTy (Boxed ty) = "\\Box\\ " ++ latexPrintMonoTy ty
latexPrintMonoTy (TypeVariable var) | var == "\alpha" = " \\alpha "
latexPrintMonoTy (TypeVariable var) = show var

data PolyType = Mono MonoType
                | TypeAbstraction Variable PolyType

freeVariablesPolyType :: PolyType -> [Variable]
freeVariablesPolyType (Mono ty) = freeVariablesMonoType ty
freeVariablesPolyType (TypeAbstraction var polyTy) = filter (\x -> x /= var) (freeVariablesPolyType polyTy)

identifyQuantifiedTypeVars :: PolyType -> ([Variable], MonoType)
identifyQuantifiedTypeVars (TypeAbstraction var ty) = 
    ([var] ++ vars, mono)
    where (vars, mono) = identifyQuantifiedTypeVars ty
identifyQuantifiedTypeVars (Mono ty) = ([], ty)

instance Show PolyType where
    show (Mono ty) = show ty
    show (TypeAbstraction var ty) | var == "\alpha" = "∀⍺." ++ show ty
    show (TypeAbstraction var ty) = "∀(" ++ show var ++ ")." ++ show ty

latexPrintPolyTy :: PolyType -> String
latexPrintPolyTy (Mono ty) = latexPrintMonoTy ty
latexPrintPolyTy (TypeAbstraction var ty) | var == "\alpha" = "∀⍺." ++ latexPrintPolyTy ty
latexPrintPolyTy (TypeAbstraction var ty) = "∀(" ++ show var ++ ")." ++ latexPrintPolyTy ty

data TypeVariableMapping = TypeVarMap [(Variable, MonoType)]

instance Show TypeVariableMapping where
    show (TypeVarMap map) = (foldl (\str -> \(ident, ty) -> str ++ show ty ++ "/" ++ mapIdent ident ++ ",") "[" map) ++ "]"
                            where   mapIdent var | var == "\alpha" = "⍺"
                                    mapIdent var = var

typeInsntiation :: PolyType -> TypeVariableMapping -> Maybe MonoType
typeInsntiation ty (TypeVarMap map) = 
    case length (boundVariables \\ [var | (var, _) <- map]) == 0 of
        True -> Just (foldl (\subject -> \(var, mono) -> (substituteInMonoType subject var mono)) mono map)
        False -> Nothing
    where (boundVariables, mono) = identifyQuantifiedTypeVars ty
