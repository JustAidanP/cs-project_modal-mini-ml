module Monomorphic.Simple.Inference where

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

typeCheck :: ModalContext -> OrdinaryContext -> Term -> Type -> Maybe CheckingError
-- B-LAM
typeCheck mctx octx tm@(Lambda var anno body) ty@(Abstraction fromType toType) | anno == fromType =
  case premise of
    Nothing -> Nothing
    Just err -> Just (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx (OCons var fromType octx) body toType
typeCheck mctx octx tm@(Lambda var anno body) ty@(Abstraction fromType toType) = Just (ChkPremiseFails 0 (PrmTypesNotEqual anno ty))
typeCheck mctx octx tm@(Lambda var anno body) ty = Just (ChkWrongForm (FmAbstraction FmMetaVariable FmMetaVariable) ty)

-- B-BOX
typeCheck mctx octx tm@(Box body) ty@(Boxed bodyType) = 
    case firstPremise of
        Nothing -> Nothing
        Just err -> Just (ChkPremiseFails 0 (PrmTypeCheckingFails err))
    where firstPremise = typeCheck mctx OEmpty body bodyType
typeCheck mctx octx tm@(Box body) ty = Just (ChkWrongForm (FmBoxed FmMetaVariable) ty)

-- B-CHECKING-LETBOX
typeCheck mctx octx tm@(LetBox var ofBody inBody) ty =
  case firstPremise of
    Left (Boxed argTy) -> 
      case secondPremise of 
        Nothing -> Nothing
        Just err -> Just (ChkPremiseFails 1 (PrmTypeCheckingFails err))
      where 
        secondPremise = typeCheck (MCons var argTy mctx) octx inBody ty
    Left argTy -> Just (ChkPremiseFails 0 (PrmSynthesisesWrongForm (FmBoxed FmMetaVariable) argTy))
    Right err -> Just (ChkPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    firstPremise = typeSynthesise mctx octx ofBody

-- B-PAIR
typeCheck mctx octx tm@(Pair left right) ty@(Product leftTy rightTy) =
  case firstPremise of
    Nothing ->
      case secondPremise of
        Nothing -> Nothing
        Just err -> Just (ChkPremiseFails 1 (PrmTypeCheckingFails err))
      where
        secondPremise = typeCheck mctx octx right rightTy
    Just err -> Just (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx left leftTy
typeCheck mctx octx tm@(Pair left right) ty = Just (ChkWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty)

-- B-UNIT
typeCheck mctx octx EmptyPair Unit = Nothing
typeCheck mctx octx EmptyPair ty = Just (ChkWrongForm (FmUnit) ty)
-- B-Z
typeCheck mctx octx Zero Natural = Nothing
typeCheck mctx octx Zero ty = Just (ChkWrongForm (FmNatural) ty)
-- B-S
typeCheck mctx octx tm@(Succ body) ty@Natural =
  case premise of
    Nothing -> Nothing
    Just err -> Just (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx octx body Natural

-- B-CHECKING-CASE
typeCheck mctx octx tm@(Case depend zero var succ) ty =
  case firstPremise of
    Nothing -> 
      case secondPremise of
        Nothing ->
          case thirdPremise of
            Nothing -> Nothing
            Just err -> Just (ChkPremiseFails 2 (PrmTypeCheckingFails err))
          where
            thirdPremise = typeCheck mctx (OCons var Natural octx) succ ty
        Just err -> Just (ChkPremiseFails 1 (PrmTypeCheckingFails err))
      where
        secondPremise = typeCheck mctx octx zero ty
    Just err -> Just (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx depend Natural

-- B-FIX
typeCheck mctx octx tm@(Fix var anno body) ty | anno == ty =
  case premise of
    Nothing -> Nothing
    Just err -> Just (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx (OCons var anno octx) body anno
typeCheck mctx octx tm@(Fix var anno body) ty = Just (ChkPremiseFails 0 (PrmTypesNotEqual anno ty))

-- B-CHANGE-DIR
typeCheck inMctx inOctx inTm inTy = 
  case premise of
      Left ty | ty == inTy -> Nothing
      Left ty -> Just (ChkSynthesisedTypeMismatch inTy ty)
      Right err -> Just (ChkCannotSynthesise err)
  where premise = typeSynthesise inMctx inOctx inTm

typeSynthesise :: ModalContext -> OrdinaryContext -> Term -> Either Type SynthesisingError
-- B-OVAR
typeSynthesise mctx octx (Var var) =
  case fromOrdinaryContext var octx of
    Just ty -> Left ty
    Nothing -> Right (SynPremiseFails 0 (PrmVariableNotInOrdinaryContext var))
-- B-APP
typeSynthesise mctx octx (Application abs arg) =
  case synthesisedAbstraction of
    Left (Abstraction from to) ->
      case checkedArgument of
        Nothing -> Left to
        Just err -> Right (SynPremiseFails 1 (PrmTypeCheckingFails err))
      where
        checkedArgument = typeCheck mctx octx arg from
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    synthesisedAbstraction = typeSynthesise mctx octx abs
-- B-MVAR
typeSynthesise mctx octx (ModalVar mvar) =
  case fromModalContext mvar mctx of
    Just ty -> Left ty
    Nothing -> Right (SynPremiseFails 0 (PrmVariableNotInModalContext mvar))
-- B-SYN-LETBOX
typeSynthesise mctx octx (LetBox var ofBody inBody) =
  case firstPremise of
    Left (Boxed ofType) ->
      case secondPremise of
        Left inType -> Left inType
        Right err -> Right (SynPremiseFails 1(PrmTypeSynthesisesFails err))
      where
        secondPremise = typeSynthesise (MCons var ofType mctx) octx inBody
    Left ty -> Right (SynPremiseFails 0 (PrmSynthesisesWrongForm (FmBoxed FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    firstPremise = typeSynthesise mctx octx ofBody
-- B-FIRST
typeSynthesise mctx octx (First body) =
  case premise of
    Left (Product left right) -> Left left
    Left ty -> Right (SynPremiseFails 0 (PrmSynthesisesWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    premise = typeSynthesise mctx octx body
-- B-SND
typeSynthesise mctx octx (Second body) =
  case premise of
    Left (Product left right) -> Left right
    Left ty -> Right (SynPremiseFails 0 (PrmSynthesisesWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    premise = typeSynthesise mctx octx body
-- B-SYN-CASE
typeSynthesise mctx octx (Case depend zero var succ) =
  case firstPremise of
    Nothing ->
      case secondPremise of
        Left zeroType ->
          case thirdPremise of
            Left succType | succType == zeroType -> Left zeroType
            Left succType -> Right (SynPremiseFails 2 (PrmTypesNotEqual zeroType succType))
            Right err -> Right (SynPremiseFails 2 (PrmTypeSynthesisesFails err))
          where
            thirdPremise = typeSynthesise mctx (OCons var Natural octx) succ
        Right err -> Right (SynPremiseFails 1 (PrmTypeSynthesisesFails err))
      where
        secondPremise = typeSynthesise mctx octx zero
    Just err -> Right (SynPremiseFails 0 (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx depend Natural
-- B-ANNO
typeSynthesise mctx octx (Anno expr ty) =
  case premise of
    Nothing -> Left ty
    Just err -> Right (SynPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx octx expr ty
-- No Rule
typeSynthesise mctx octx _ = Right (SynNoRule)

data Error = SynError SynthesisingError | ChkError CheckingError

-- giveErrorMessage :: ModalContext -> OrdinaryContext -> Term -> Error
-- giveErrorMessage

infer :: ModalContext -> OrdinaryContext -> Term -> Either Type SynthesisingError
infer mctx octx tm = typeSynthesise mctx octx tm