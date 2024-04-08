module Monomorphic.Terms where

import Monomorphic.Types

data Term = Var Variable                    -- x
            | Lambda Variable Type Term     -- 𝛌x:A.E
            | Application Term Term         -- E E'
            | ModalVar Variable             -- u
            | Box Term                      -- box E
            | LetBox Variable Term Term     -- let box u = E in E'
            | Pair Term Term                -- <E, E'>
            | First Term                    -- fst E
            | Second Term                   -- snd E
            | EmptyPair                     -- <>
            | Zero                          -- Z
            | Succ Term                     -- s E
            | Case Term Term Variable Term  -- (case E of z => E' | s x => E'')
            | Fix Variable Type Term        -- fix x:A.E
            | Anno Term Type                -- (E : A)

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
    show (Succ prev) = "(s " ++ show prev ++ ")"
    show (Case depend zeroCase ident succCase) = "(case " ++ show depend ++ " of z => " ++ show zeroCase ++ " | s " ++ ident ++ " => " ++ show succCase ++")"
    show (Fix funcIdent funcTy body) = "fix " ++ funcIdent ++ ":" ++ show funcTy ++ "." ++ show body
    show (Anno term termTy) = "(" ++ show term ++ " : " ++ show termTy ++")"

latexPrintTm :: Term -> String
latexPrintTm (Var ident) = "\\underline{" ++ ident ++ "}"
latexPrintTm (Lambda arg argType body) = "\\lambda" ++ "\\underline{" ++ arg ++ "} \\mathbin{:} " ++ latexPrintTy argType ++ "." ++ latexPrintTm body
latexPrintTm (Application abs to) = "(" ++ latexPrintTm abs ++ ")" ++ latexPrintTm to
latexPrintTm (ModalVar ident) = "\\underline{" ++ ident ++ "}"
latexPrintTm (Box code) = "\\textbf{box}\\ " ++ latexPrintTm code
latexPrintTm (LetBox ident boxed body) = "\\textbf{let box}\\ " ++ "\\underline{" ++ ident ++ "}" ++ " = " ++ latexPrintTm boxed ++ "\\ \\textbf{in}\\ " ++ latexPrintTm body
latexPrintTm (Pair left right) = "⟨" ++ latexPrintTm left ++ "," ++ latexPrintTm right ++ "⟩"
latexPrintTm (First pair) = "\\textbf{fst}\\ " ++ latexPrintTm pair
latexPrintTm (Second pair) = "\\textbf{snd}\\ " ++ latexPrintTm pair
latexPrintTm (EmptyPair) = "⟨⟩"
latexPrintTm (Zero) = "\\textbf{z}"
latexPrintTm (Succ prev) = "(\\textbf{s}\\ " ++ latexPrintTm prev ++ ")"
latexPrintTm (Case depend zeroCase ident succCase) = "(\\textbf{case}\\ " ++ latexPrintTm depend ++ " \\ \\textbf{of}\\ z \\Rightarrow " ++ latexPrintTm zeroCase ++ " \\mid s\\ " ++ "\\underline{" ++ ident ++ "}" ++ " \\Rightarrow " ++ latexPrintTm succCase ++")"
latexPrintTm (Fix funcIdent funcTy body) = "\\textbf{fix}\\ " ++ "\\underline{" ++ funcIdent ++ "}" ++ "\\mathbin{:}" ++ latexPrintTy funcTy ++ "." ++ latexPrintTm body
latexPrintTm (Anno term termTy) = "(" ++ latexPrintTm term ++ " \\mathbin{:} " ++ latexPrintTy termTy ++")"