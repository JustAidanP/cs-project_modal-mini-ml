module Monomorphic.Derivation where

import Monomorphic.Types
import Monomorphic.Terms




data Derivation = TypeChecks [Derivation] Term Type | TypeSynthesises [Derivation] Term Type 
                | DoesNotCheck [Derivation] Term Type | DoesNotSynthesise [Derivation] Term
                | NoDerivation
    deriving (Show)