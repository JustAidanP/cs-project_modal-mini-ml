module Main (main) where

import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Bidirectional
import Monomorphic.Derivation
import Monomorphic.Context

-- (\x:1.<>) <>
test = Application (Lambda "x" Unit EmptyPair) EmptyPair

-- let box u = box \x:1.x in u <>

-- An example, taken from Davies and Pfenning, "A Modal Analysis of Staged Computation"
power = Fix "p" (Abstraction Natural (Boxed (Abstraction Natural Natural))) 
            (Lambda "n" Natural 
                (Case (Var "n") 
                    (Box (Lambda "x" Natural (Succ Zero))) 
                    "m" (LetBox "q" (Application (Var "p") (Var "m")) 
                            (Box (Lambda "x" Natural (Application (Application (Var "times") (Var "x")) (Application (ModalVar "q") (Var "x"))))))))

main :: IO ()
main = print "Hello World"
