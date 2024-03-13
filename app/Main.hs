module Main (main) where

import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Bidirectional
import Monomorphic.Derivation
import Monomorphic.Context

-- (\x:1.<>) <>
test = Application (Lambda "x" Unit EmptyPair) EmptyPair

-- let box u = box \x:1.x in u <>


main :: IO ()
main = print "Hello World"
