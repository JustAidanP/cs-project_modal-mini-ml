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

times = LetBox "plus" (Anno (Box plus) (Boxed (Abstraction Natural (Abstraction Natural Natural))))
            (Lambda "n" Natural 
                (Fix "t" (Abstraction Natural Natural)
                    (Lambda "m" Natural 
                        (Case (Var "m") 
                            Zero
                            "x" (Application (Application (ModalVar "plus") (Var "n")) (Application (Var "t") (Var "x")))))))

-- An example, taken from Davies and Pfenning, "A Modal Analysis of Staged Computation"
power = LetBox "times" (Anno (Box times) (Boxed (Abstraction Natural (Abstraction Natural Natural)))) 
            (Fix "p" (Abstraction Natural (Boxed (Abstraction Natural Natural))) 
                (Lambda "n" Natural 
                    (Case (Var "n") 
                        (Box (Lambda "x" Natural (Succ Zero))) 
                        "m" (LetBox "q" (Application (Var "p") (Var "m")) 
                                (Box (Lambda "x" Natural (Application (Application (ModalVar "times") (Var "x")) (Application (ModalVar "q") (Var "x")))))))))

-- λ typeSynthesising MEmpty OEmpty (Anno power (Abstraction Natural (Boxed (Abstraction Natural Natural))))
--  | | | | | | | | | | | | = ·; m:nat, p:(nat -> nat), n:nat, · ├ m => nat
--  | | | | | | | | | | | = ·; m:nat, p:(nat -> nat), n:nat, · ├ m <= nat
--  | | | | | | | | | | | | = ·; m:nat, p:(nat -> nat), n:nat, · ├ n => nat
--  | | | | | | | | | | | = ·; m:nat, p:(nat -> nat), n:nat, · ├ n <= nat
--  | | | | | | | | | | | | | | = ·; x:nat, m:nat, p:(nat -> nat), n:nat, · ├ p => (nat -> nat)
--  | | | | | | | | | | | | | | | = ·; x:nat, m:nat, p:(nat -> nat), n:nat, · ├ x => nat
--  | | | | | | | | | | | | | | = ·; x:nat, m:nat, p:(nat -> nat), n:nat, · ├ x <= nat
--  | | | | | | | | | | | | | = ·; x:nat, m:nat, p:(nat -> nat), n:nat, · ├ (p)x => nat
--  | | | | | | | | | | | | = ·; x:nat, m:nat, p:(nat -> nat), n:nat, · ├ (p)x <= nat
--  | | | | | | | | | | | = ·; x:nat, m:nat, p:(nat -> nat), n:nat, · ├ s (p)x <= nat
--  | | | | | | | | | | = ·; m:nat, p:(nat -> nat), n:nat, · ├ (case m of z => n | s x => s (p)x) <= nat
--  | | | | | | | | | = ·; p:(nat -> nat), n:nat, · ├ 𝛌m:nat.(case m of z => n | s x => s (p)x) <= (nat -> nat)
--  | | | | | | | | = ·; n:nat, · ├ fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) <= (nat -> nat)
--  | | | | | | | = ·; · ├ 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) <= (nat -> (nat -> nat))
--  | | | | | | = ·; · ├ box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) <= ☐(nat -> (nat -> nat))
--  | | | | | = ·; · ├ (box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) : ☐(nat -> (nat -> nat))) => ☐(nat -> (nat -> nat))
--  | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; m:nat, t:(nat -> nat), n:nat, · ├ m => nat
--  | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; m:nat, t:(nat -> nat), n:nat, · ├ m <= nat
--  | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; m:nat, t:(nat -> nat), n:nat, · ├ z <= nat
--  | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ plus => (nat -> (nat -> nat))
--  | | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ n => nat
--  | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ n <= nat
--  | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ (plus)n => (nat -> nat)
--  | | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ t => (nat -> nat)
--  | | | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ x => nat
--  | | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ x <= nat
--  | | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ (t)x => nat
--  | | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ (t)x <= nat
--  | | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ ((plus)n)(t)x => nat
--  | | | | | | | | | = plus:(nat -> (nat -> nat)), ·; x:nat, m:nat, t:(nat -> nat), n:nat, · ├ ((plus)n)(t)x <= nat
--  | | | | | | | | = plus:(nat -> (nat -> nat)), ·; m:nat, t:(nat -> nat), n:nat, · ├ (case m of z => z | s x => ((plus)n)(t)x) <= nat
--  | | | | | | | = plus:(nat -> (nat -> nat)), ·; t:(nat -> nat), n:nat, · ├ 𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) <= (nat -> nat)
--  | | | | | | = plus:(nat -> (nat -> nat)), ·; n:nat, · ├ fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) <= (nat -> nat)
--  | | | | | = plus:(nat -> (nat -> nat)), ·; · ├ 𝛌n:nat.fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) <= (nat -> (nat -> nat))
--  | | | | = ·; · ├ let box plus = (box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) : ☐(nat -> (nat -> nat))) in 𝛌n:nat.fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) <= (nat -> (nat -> nat))
--  | | | = ·; · ├ box let box plus = (box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) : ☐(nat -> (nat -> nat))) in 𝛌n:nat.fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) <= ☐(nat -> (nat -> nat))
--  | | = ·; · ├ (box let box plus = (box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) : ☐(nat -> (nat -> nat))) in 𝛌n:nat.fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) : ☐(nat -> (nat -> nat))) => ☐(nat -> (nat -> nat))
--  | | | | | | = times:(nat -> (nat -> nat)), ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ n => nat
--  | | | | | = times:(nat -> (nat -> nat)), ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ n <= nat
--  | | | | | | | | = times:(nat -> (nat -> nat)), ·; x:nat, · ├ z <= nat
--  | | | | | | | = times:(nat -> (nat -> nat)), ·; x:nat, · ├ s z <= nat
--  | | | | | | = times:(nat -> (nat -> nat)), ·; · ├ 𝛌x:nat.s z <= (nat -> nat)
--  | | | | | = times:(nat -> (nat -> nat)), ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ box 𝛌x:nat.s z <= ☐(nat -> nat)
--  | | | | | | | = times:(nat -> (nat -> nat)), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ p => (nat -> ☐(nat -> nat))
--  | | | | | | | | = times:(nat -> (nat -> nat)), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ m => nat
--  | | | | | | | = times:(nat -> (nat -> nat)), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ m <= nat
--  | | | | | | = times:(nat -> (nat -> nat)), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ (p)m => ☐(nat -> nat)
--  | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ times => (nat -> (nat -> nat))
--  | | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ x => nat
--  | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ x <= nat
--  | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ (times)x => (nat -> nat)
--  | | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ q => (nat -> nat)
--  | | | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ x => nat
--  | | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ x <= nat
--  | | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ (q)x => nat
--  | | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ (q)x <= nat
--  | | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ ((times)x)(q)x => nat
--  | | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; x:nat, · ├ ((times)x)(q)x <= nat
--  | | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; · ├ 𝛌x:nat.((times)x)(q)x <= (nat -> nat)
--  | | | | | | = q:(nat -> nat), times:(nat -> (nat -> nat)), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ box 𝛌x:nat.((times)x)(q)x <= ☐(nat -> nat)
--  | | | | | = times:(nat -> (nat -> nat)), ·; m:nat, n:nat, p:(nat -> ☐(nat -> nat)), · ├ let box q = (p)m in box 𝛌x:nat.((times)x)(q)x <= ☐(nat -> nat)
--  | | | | = times:(nat -> (nat -> nat)), ·; n:nat, p:(nat -> ☐(nat -> nat)), · ├ (case n of z => box 𝛌x:nat.s z | s m => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <= ☐(nat -> nat)
--  | | | = times:(nat -> (nat -> nat)), ·; p:(nat -> ☐(nat -> nat)), · ├ 𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s m => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <= (nat -> ☐(nat -> nat))
--  | | = times:(nat -> (nat -> nat)), ·; · ├ fix p:(nat -> ☐(nat -> nat)).𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s m => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <= (nat -> ☐(nat -> nat))
--  | = ·; · ├ let box times = (box let box plus = (box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) : ☐(nat -> (nat -> nat))) in 𝛌n:nat.fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) : ☐(nat -> (nat -> nat))) in fix p:(nat -> ☐(nat -> nat)).𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s m => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) <= (nat -> ☐(nat -> nat))
--  = ·; · ├ (let box times = (box let box plus = (box 𝛌n:nat.fix p:(nat -> nat).𝛌m:nat.(case m of z => n | s x => s (p)x) : ☐(nat -> (nat -> nat))) in 𝛌n:nat.fix t:(nat -> nat).𝛌m:nat.(case m of z => z | s x => ((plus)n)(t)x) : ☐(nat -> (nat -> nat))) in fix p:(nat -> ☐(nat -> nat)).𝛌n:nat.(case n of z => box 𝛌x:nat.s z | s m => let box q = (p)m in box 𝛌x:nat.((times)x)(q)x) : (nat -> ☐(nat -> nat))) => (nat -> ☐(nat -> nat))

base_5 = Application (Anno power (Abstraction Natural (Boxed (Abstraction Natural Natural)))) (Succ (Succ (Succ (Succ (Succ Zero)))))


main :: IO ()
main = print "Hello World"
