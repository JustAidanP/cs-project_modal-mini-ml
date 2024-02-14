module Main (main) where

import Types
import Terms

test = Application {
    lambda = Lambda {
        variable = "x",
        variable_annotation = Unit,
        body = EmptyProduct
    },
    argument = EmptyProduct
}



main :: IO ()
main = print "Hello World"
