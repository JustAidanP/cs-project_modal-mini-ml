module Polymorphic(
    identity,
    identityPair
) where

import Polymorphic.Types
import Polymorphic.Terms
import Polymorphic.Context
import qualified Polymorphic.Simple.Inference

identity = Lambda "x" (TypeVariable "\alpha") (Var "x")
identityPair = LetBox 
                    "u" 
                    (Anno (Box identity) (Boxed (Abstraction (TypeVariable "\alpha") (TypeVariable "\alpha")))) 
                    (Anno (Pair 
                            (InstModalVar "u" (TypeVarMap [("\alpha", Natural)])) 
                            (InstModalVar "u" (TypeVarMap [("\alpha", Unit)]))) 
                        (Product (Abstraction Natural Natural) (Abstraction Unit Unit)))