module Monomorphic.Annotated.Inference where

import Monomorphic.Context
import Monomorphic.Terms
import Monomorphic.Types
import Monomorphic.Error

import Data.List

data DerivationTree = Checks [DerivationTree] ModalContext OrdinaryContext Term Type
                | Synthesises [DerivationTree] ModalContext OrdinaryContext Term Type
    deriving (Show)

latexPrintTree :: DerivationTree -> String
latexPrintTree (Checks premises mctx octx tm ty) = 
    "\\inferrule*"
        -- ++ "[right=" ++ show rule ++ "]"
        ++"{" ++ (intercalate " " (map latexPrintTree premises))  ++ "}{" 
        ++ latexPrintMCtx mctx ++ "; " ++ latexPrintOCtx octx ++ " \\vdash " ++ latexPrintTm tm ++ " \\Leftarrow " ++ latexPrintTy ty ++ "}"
latexPrintTree (Synthesises premises mctx octx tm ty) = 
    "\\inferrule*"
    -- ++ "[right=" ++ show rule ++ "]"
    ++"{" ++ (intercalate " " (map latexPrintTree premises))  ++ "}{" 
    ++ latexPrintMCtx mctx ++ "; " ++ latexPrintOCtx octx ++ " \\vdash " ++ latexPrintTm tm ++ " \\Rightarrow " ++ latexPrintTy ty ++ "}"

typeCheck :: ModalContext -> OrdinaryContext -> Term -> Type -> Either DerivationTree CheckingError
-- B-LAM
typeCheck mctx octx tm@(Lambda var anno body) ty@(Abstraction fromType toType) | anno == fromType =
  case premise of
    Left branch -> Left (Checks [branch] mctx octx tm ty)
    Right err -> Right (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx (OCons var fromType octx) body toType
typeCheck mctx octx tm@(Lambda var anno body) ty@(Abstraction fromType toType) = Right (ChkPremiseFails 0 (PrmTypesNotEqual anno ty))
typeCheck mctx octx tm@(Lambda var anno body) ty = Right (ChkWrongForm (FmAbstraction FmMetaVariable FmMetaVariable) ty)

-- B-BOX
typeCheck mctx octx tm@(Box body) ty@(Boxed bodyType) = 
    case firstPremise of
        Left branch -> Left (Checks [branch] mctx octx tm ty)
        Right err -> Right (ChkPremiseFails 0 (PrmTypeCheckingFails err))
    where firstPremise = typeCheck mctx OEmpty body bodyType
typeCheck mctx octx tm@(Box body) ty = Right (ChkWrongForm (FmBoxed FmMetaVariable) ty)

-- B-CHECKING-LETBOX
typeCheck mctx octx tm@(LetBox var ofBody inBody) ty =
  case firstPremise of
    Left (firstBranch, (Boxed argTy)) -> 
      case secondPremise of 
        Left secondBranch -> Left (Checks [firstBranch, secondBranch] mctx octx tm ty)
        Right err -> Right (ChkPremiseFails 1 (PrmTypeCheckingFails err))
      where 
        secondPremise = typeCheck (MCons var argTy mctx) octx inBody ty
    Left (_, argTy) -> Right (ChkPremiseFails 0 (PrmSynthesisesWrongForm (FmBoxed FmMetaVariable) argTy))
    Right err -> Right (ChkPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    firstPremise = typeSynthesise mctx octx ofBody

-- B-PAIR
typeCheck mctx octx tm@(Pair left right) ty@(Product leftTy rightTy) =
  case firstPremise of
    Left firstBranch ->
      case secondPremise of
        Left secondBranch -> Left (Checks [firstBranch, secondBranch] mctx octx tm ty)
        Right err -> Right (ChkPremiseFails 1 (PrmTypeCheckingFails err))
      where
        secondPremise = typeCheck mctx octx right rightTy
    Right err -> Right (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx left leftTy
typeCheck mctx octx tm@(Pair left right) ty = Right (ChkWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty)

-- B-UNIT
typeCheck mctx octx EmptyPair Unit = Left (Checks [] mctx octx EmptyPair Unit)
typeCheck mctx octx EmptyPair ty = Right (ChkWrongForm (FmUnit) ty)
-- B-Z
typeCheck mctx octx Zero Natural = Left (Checks [] mctx octx Zero Natural)
typeCheck mctx octx Zero ty = Right (ChkWrongForm (FmNatural) ty)
-- B-S
typeCheck mctx octx tm@(Succ body) ty@Natural =
  case premise of
    Left branch -> Left (Checks [branch] mctx octx tm ty)
    Right err -> Right (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx octx body Natural

-- B-CHECKING-CASE
typeCheck mctx octx tm@(Case depend zero var succ) ty =
  case firstPremise of
    Left firstBranch -> 
      case secondPremise of
        Left secondBranch ->
          case thirdPremise of
            Left thirdBranch -> Left (Checks [firstBranch, secondBranch, thirdBranch] mctx octx tm ty)
            Right err -> Right (ChkPremiseFails 2 (PrmTypeCheckingFails err))
          where
            thirdPremise = typeCheck mctx (OCons var Natural octx) succ ty
        Right err -> Right (ChkPremiseFails 1 (PrmTypeCheckingFails err))
      where
        secondPremise = typeCheck mctx octx zero ty
    Right err -> Right (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx depend Natural

-- B-FIX
typeCheck mctx octx tm@(Fix var anno body) ty | anno == ty =
  case premise of
    Left branch -> Left (Checks [branch] mctx octx tm ty)
    Right err -> Right (ChkPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx (OCons var anno octx) body anno
typeCheck mctx octx tm@(Fix var anno body) ty = Right (ChkPremiseFails 0 (PrmTypesNotEqual anno ty))

-- B-CHANGE-DIR
typeCheck inMctx inOctx inTm inTy = 
  case premise of
      Left (branch, ty) | ty == inTy -> Left (Checks [branch] inMctx inOctx inTm inTy)
      Left (_, ty)
       -> Right (ChkSynthesisedTypeMismatch inTy ty)
      Right err -> Right (ChkCannotSynthesise err)
  where premise = typeSynthesise inMctx inOctx inTm

typeSynthesise :: ModalContext -> OrdinaryContext -> Term -> Either (DerivationTree, Type) SynthesisingError
-- B-OVAR
typeSynthesise mctx octx (Var var) =
  case fromOrdinaryContext var octx of
    Just ty -> Left ((Synthesises [] mctx octx (Var var) ty), ty)
    Nothing -> Right (SynPremiseFails 0 (PrmVariableNotInOrdinaryContext var))
-- B-APP
typeSynthesise mctx octx tm@(Application abs arg) =
  case synthesisedAbstraction of
    Left (firstBranch, (Abstraction from to)) ->
      case checkedArgument of
        Left secondBranch -> Left ((Synthesises [firstBranch, secondBranch] mctx octx tm to), to)
        Right err -> Right (SynPremiseFails 1 (PrmTypeCheckingFails err))
      where
        checkedArgument = typeCheck mctx octx arg from
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    synthesisedAbstraction = typeSynthesise mctx octx abs
-- B-MVAR
typeSynthesise mctx octx (ModalVar mvar) =
  case fromModalContext mvar mctx of
    Just ty -> Left ((Synthesises [] mctx octx (ModalVar mvar) ty), ty)
    Nothing -> Right (SynPremiseFails 0 (PrmVariableNotInModalContext mvar))
-- B-SYN-LETBOX
typeSynthesise mctx octx tm@(LetBox var ofBody inBody) =
  case firstPremise of
    Left (firstBranch, (Boxed ofType)) ->
      case secondPremise of
        Left (secondBranch, inType) -> Left ((Synthesises [firstBranch, secondBranch] mctx octx tm inType), inType)
        Right err -> Right (SynPremiseFails 1(PrmTypeSynthesisesFails err))
      where
        secondPremise = typeSynthesise (MCons var ofType mctx) octx inBody
    Left (_, ty) -> Right (SynPremiseFails 0 (PrmSynthesisesWrongForm (FmBoxed FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    firstPremise = typeSynthesise mctx octx ofBody
-- B-FIRST
typeSynthesise mctx octx tm@(First body) =
  case premise of
    Left (branch, (Product left right)) -> Left ((Synthesises [branch] mctx octx tm left), left)
    Left (_, ty) -> Right (SynPremiseFails 0 (PrmSynthesisesWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    premise = typeSynthesise mctx octx body
-- B-SND
typeSynthesise mctx octx tm@(Second body) =
  case premise of
    Left (branch, (Product left right)) -> Left ((Synthesises [branch] mctx octx tm right), right)
    Left (_, ty) -> Right (SynPremiseFails 0 (PrmSynthesisesWrongForm (FmProduct FmMetaVariable FmMetaVariable) ty))
    Right err -> Right (SynPremiseFails 0 (PrmTypeSynthesisesFails err))
  where
    premise = typeSynthesise mctx octx body
-- B-SYN-CASE
typeSynthesise mctx octx tm@(Case depend zero var succ) =
  case firstPremise of
    Left firstBranch ->
      case secondPremise of
        Left (secondBranch, zeroType) ->
          case thirdPremise of
            Left (thirdBranch, succType) | succType == zeroType -> Left ((Synthesises [firstBranch, secondBranch, thirdBranch] mctx octx tm zeroType), zeroType)
            Left (_, succType) -> Right (SynPremiseFails 2 (PrmTypesNotEqual zeroType succType))
            Right err -> Right (SynPremiseFails 2 (PrmTypeSynthesisesFails err))
          where
            thirdPremise = typeSynthesise mctx (OCons var Natural octx) succ
        Right err -> Right (SynPremiseFails 1 (PrmTypeSynthesisesFails err))
      where
        secondPremise = typeSynthesise mctx octx zero
    Right err -> Right (SynPremiseFails 0 (PrmTypeCheckingFails err))
  where
    firstPremise = typeCheck mctx octx depend Natural
-- B-ANNO
typeSynthesise mctx octx tm@(Anno expr ty) =
  case premise of
    Left branch -> Left ((Synthesises [branch] mctx octx tm ty),
     ty)
    Right err -> Right (SynPremiseFails 0 (PrmTypeCheckingFails err))
  where
    premise = typeCheck mctx octx expr ty
-- No Rule
typeSynthesise mctx octx _ = Right (SynNoRule)

infer :: ModalContext -> OrdinaryContext -> Term -> Either Type SynthesisingError
infer mctx octx tm = case typeSynthesise mctx octx tm of
                        Left (_, ty) -> Left ty
                        Right err -> Right err