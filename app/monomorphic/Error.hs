module Monomorphic.Error where

import Monomorphic.Context
import Monomorphic.Terms
import Monomorphic.Types

data Form = FmNatural | FmAbstraction Form Form | FmProduct Form Form | FmUnit | FmBoxed Form | FmMetaVariable
  deriving (Show, Eq)

data PremiseError
  = PrmVariableNotInOrdinaryContext Variable -- Ordinary Variable not found
  | PrmVariableNotInModalContext Variable -- Modal Variable not found
  | PrmTypesNotEqual Type Type -- Type Mismatch
  | PrmTypeCheckingFails CheckingError
  | PrmTypeSynthesisesFails SynthesisingError
  | PrmSynthesisesWrongForm Form Type -- This term has a type of an unexpected form
  deriving (Show)

data CheckingError
  = ChkWrongForm Form Type -- The type of this term cannot be of this form
  | ChkPremiseFails Integer PremiseError
  | ChkSynthesisedTypeMismatch Type Type -- We found that this term has a different type to what was expected
  | ChkCannotSynthesise SynthesisingError -- We can't check the type of this term, nor can we find its type
  deriving (Show)

data SynthesisingError
  = SynNoRule -- Annotation Required
  | SynPremiseFails Integer PremiseError
  deriving (Show)