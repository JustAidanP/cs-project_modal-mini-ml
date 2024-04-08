module Monomorphic.Simple.Inference where

import Monomorphic.Context
import Monomorphic.Terms
import Monomorphic.Types

data Form = FmNatural | FmAbstraction Form Form | FmProduct Form Form | FmUnit | FmBoxed Form | FmMetaVariable
  deriving (Show, Eq)

data PremiseError
  = PrmVariableNotInOrdinaryContext Variable
  | PrmVariableNotInModalContext Variable
  | PrmTypesNotEqual Type Type
  | PrmTypeCheckingFails CheckingError
  | PrmTypeSynthesisesFails SynthesisingError
  | PrmSynthesisesWrongForm Form Type
  deriving (Show)

data CheckingError
  = ChkWrongForm Form Type
  | ChkPremiseFails PremiseError
  | ChkSynthesisedTypeMismatch Type Type
  | ChkCannotSynthesise SynthesisingError
  deriving (Show)

data SynthesisingError
  = SynNoRule
  | SynPremiseFails PremiseError
  deriving (Show)

typeCheck :: ModalContext -> OrdinaryContext -> Term -> Type -> Maybe CheckingError
typeCheck _ _ _ _ = Nothing
-- B-LAM
typeCheck mctx octx tm@(Lambda var anno body) ty@(Abstraction fromType toType) | anno == fromType =
  case premise of
    Nothing -> Nothing
    Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx (OCons var fromType octx) body toType
typeCheck mctx octx tm@(Lambda var anno body) ty@(Abstraction fromType toType) = Just (ChkPremiseFails (PrmTypesNotEqual anno ty))
typeCheck mctx octx tm@(Lambda var anno body) ty = Just (ChkWrongForm (FmAbstraction FmMetaVariable FmMetaVariable) ty)

-- B-BOX
typeCheck mctx octx tm@(Box body) ty@(Boxed bodyType) = 
    case firstPremise of
        Nothing -> Nothing
        Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
    where firstPremise = typeCheck mctx OEmpty body bodyType
typeCheck mctx octx tm@(Box body) ty = Just (ChkWrongForm (FmBoxed FmMetaVariable) ty)

-- B-CHECKING-LETBOX
typeCheck mctx octx tm@(LetBox var ofBody inBody) ty =
  case firstPremise of
    Left (Boxed argTy) -> 
      case secondPremise of 
        Nothing -> Nothing
        Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
      where 
        secondPremise = typeCheck (MCons var argTy mctx) octx inBody ty
    Left argTy -> Just (ChkPremiseFails (PrmSynthesisesWrongForm (FmBoxed FmMetaVariable) argTy))
    Right err -> Just (ChkPremiseFails (PrmTypeSynthesisesFails err))
  where
    firstPremise = typeSynthesise mctx octx ofBody

-- B-PAIR
typeCheck mctx octx tm@(Pair left right) ty@(Product leftTy rightTy) =
  case firstPremise of
    Nothing ->
      case secondPremise of
        Nothing -> Nothing
        Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
      where
        secondPremise = typeCheck mctx octx right rightTy
    Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
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
    Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
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
            Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
          where
            thirdPremise = typeCheck mctx (OCons var Natural octx) succ ty
        Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
      where
        secondPremise = typeCheck mctx octx zero ty
    Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx depend Natural

-- B-FIX
typeCheck mctx octx tm@(Fix var anno body) ty | anno == ty =
  case premise of
    Nothing -> Nothing
    Just err -> Just (ChkPremiseFails (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx (OCons var anno octx) body anno
typeCheck mctx octx tm@(Fix var anno body) ty = Just (ChkPremiseFails (PrmTypesNotEqual anno ty))

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
    Nothing -> Right (SynPremiseFails (PrmVariableNotInOrdinaryContext var))
-- B-APP
typeSynthesise mctx octx (Application abs arg) =
  case synthesisedAbstraction of
    Left (Abstraction from to) ->
      case checkedArgument of
        Nothing -> Left to
        Just err -> Right (SynPremiseFails (PrmTypeCheckingFails err))
      where
        checkedArgument = typeCheck mctx octx arg from
    Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
  where
    synthesisedAbstraction = typeSynthesise mctx octx abs
-- B-MVAR
typeSynthesise mctx octx (ModalVar mvar) =
  case fromModalContext mvar mctx of
    Just ty -> Left ty
    Nothing -> Right (SynPremiseFails (PrmVariableNotInModalContext mvar))
-- B-SYN-LETBOX
typeSynthesise mctx octx (LetBox var ofBody inBody) =
  case firstPremise of
    Left (Boxed ofType) ->
      case secondPremise of
        Left inType -> Left inType
        Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
      where
        secondPremise = typeSynthesise (MCons var ofType mctx) octx inBody
    Left ty -> Right (SynPremiseFails (PrmSynthesisesWrongForm (FmBoxed FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
  where
    firstPremise = typeSynthesise mctx octx ofBody
-- B-FIRST
typeSynthesise mctx octx (First body) =
  case premise of
    Left (Product left right) -> Left left
    Left ty -> Right (SynPremiseFails (PrmSynthesisesWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
  where
    premise = typeSynthesise mctx octx body
-- B-SND
typeSynthesise mctx octx (Second body) =
  case premise of
    Left (Product left right) -> Left right
    Left ty -> Right (SynPremiseFails (PrmSynthesisesWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
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
            Left succType -> Right (SynPremiseFails (PrmTypesNotEqual zeroType succType))
            Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
          where
            thirdPremise = typeSynthesise mctx (OCons var Natural octx) succ
        Right err -> Right (SynPremiseFails (PrmTypeSynthesisesFails err))
      where
        secondPremise = typeSynthesise mctx octx zero
    Just err -> Right (SynPremiseFails (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx depend Natural
-- B-ANNO
typeSynthesise mctx octx (Anno expr ty) =
  case premise of
    Nothing -> Left ty
    Just err -> Right (SynPremiseFails (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx octx expr ty
-- No Rule
typeSynthesise mctx octx _ = Right (SynNoRule)

infer :: ModalContext -> OrdinaryContext -> Term -> Either Type SynthesisingError
infer mctx octx tm = typeSynthesise mctx octx tm