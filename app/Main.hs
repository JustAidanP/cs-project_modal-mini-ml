module Main (main) where

import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Bidirectional
import Monomorphic.Derivation
import Monomorphic.Context

-- (\x:1.<>) <>
test = Application (Lambda "x" Unit EmptyPair) EmptyPair

-- let box u = box \x:1.x in u <>

plus = Lambda "n" Natural 
            (Fix "p" (Abstraction Natural Natural)
                (Lambda "m" Natural 
                    (Case (Var "m") 
                        (Var "n")
                        "x" (Succ (Application (Var "p") (Var "x"))))))

times = Lambda "n" Natural 
            (Fix "t" (Abstraction Natural Natural)
                (Lambda "m" Natural 
                    (Case (Var "m") 
                        Zero
                        "x" (Application (Application (Anno plus (Abstraction Natural (Abstraction Natural Natural))) (Var "n")) (Application (Var "t") (Var "x"))))))

-- An example, taken from Davies and Pfenning, "A Modal Analysis of Staged Computation"
power = Fix "p" (Abstraction Natural (Boxed (Abstraction Natural Natural))) 
            (Lambda "n" Natural 
                (Case (Var "n") 
                    (Box (Lambda "x" Natural (Succ Zero))) 
                    "m" (LetBox "q" (Application (Var "p") (Var "m")) 
                            (Box (Lambda "x" Natural (Application (Application (Anno times (Abstraction Natural (Abstraction Natural Natural))) (Var "x")) (Application (ModalVar "q") (Var "x"))))))))

-- λ typeSynthesising MEmpty OEmpty (Anno power (Abstraction Natural (Boxed (Abstraction Natural Natural))))
--  | | | | | = ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ n => nat
--  | | | | = ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ n <= nat
--  | | | | | | | = ·; x:nat, · ├ z <= nat
--  | | | | | | = ·; x:nat, · ├ s z <= nat
--  | | | | | = ·; · ├ 𝛌x:nat.s z <= (nat -> nat)
--  | | | | = ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ box 𝛌x:nat.s z <= ☐(nat -> nat)
--  | | | | | | = ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ p => (nat -> ☐(nat -> nat))
--  | | | | | | | = ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ m => nat
--  | | | | | | = ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ m <= nat
--  | | | | | = ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ (p)m => ☐(nat -> nat)
--  | | | | | | | | | | = q:(nat -> nat), ·; x:nat, · ├ times =\>
--  | | | | | | | | | | = No Derivation
--  | | | | | | | | | = q:(nat -> nat), ·; x:nat, · ├ (times)x =\>
--  | | | | | | | | | = No Derivation
--  | | | | | | | | = q:(nat -> nat), ·; x:nat, · ├ ((times)x)(q)x =\>
--  | | | | | | | = q:(nat -> nat), ·; x:nat, · ├ ((times)x)(q)x <\= nat
--  | | | | | | = q:(nat -> nat), ·; · ├ 𝛌x:nat.((times)x)(q)x <\= (nat -> nat)
--  | | | | | = q:(nat -> nat), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ box 𝛌x:nat.((times)x)(q)x <\= ☐(nat -> nat)
--  | | | | = ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ let box q = (p)m in box 𝛌x:nat.((times)x)(q)x <\= ☐(nat -> nat)
--  | | | = ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ (case n of z => box 𝛌x:nat.s z | s "m" => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <\= ☐(nat -> nat)
--  | | = ·; p:(nat -> ☐(nat -> nat)), · ├ 𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s "m" => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <\= (nat -> ☐(nat -> nat))
--  | = ·; · ├ fix "p":(nat -> ☐(nat -> nat)).𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s "m" => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <\= (nat -> ☐(nat -> nat))
--  = ·; · ├ (fix "p":(nat -> ☐(nat -> nat)).𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s "m" => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) : (nat -> ☐(nat -> nat))) <\= (nat -> ☐(nat -> nat))

main :: IO ()
main = print "Hello World"
