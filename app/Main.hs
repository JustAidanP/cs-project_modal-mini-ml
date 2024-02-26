module Main (main) where

import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Bidirectional

test = Application (Lambda "x" Unit EmptyProduct) EmptyProduct



main :: IO ()
main = print "Hello World"
