-- We implement the (first) bidirectional rules for the Modal Mini ML [TODO: We need to name this first system]

module Monomorphic.Bidirectional where
import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Derivation
import Monomorphic.Context

-- We first implement all of the Type Checking rules
typeChecking :: ModalContext -> OrdinaryContext -> Term -> Type -> Derivation
typeChecking inMctx inOctx inTm inTy = 
    let check = doTypeChecking inMctx inOctx inTm inTy in
        case check of
            -- If we could check, then we are happy
            TypeChecks _ _ _ -> check
            -- We couldn't tyoe check, so we do B-CHANGE-DIR, i.e. we try to syntehsise
            _ -> let synth = typeSynthesising inMctx inOctx inTm in
                    case synth of
                        TypeSynthesises _ _ sty | sty == inTy -> TypeChecks [synth] inTm inTy
                        -- TODO Possible an an error message that we couldn't synth a type either?
                        _ -> check

    where   doTypeChecking :: ModalContext -> OrdinaryContext -> Term -> Type -> Derivation
            -- B-LAM
            doTypeChecking mctx octx tm@(Lambda var anno body) ty@(Abstraction fromty toty) | anno == fromty = 
                case firstPremise of
                    TypeChecks _ _ subtype -> TypeChecks [firstPremise] tm ty
                    _ -> DoesNotCheck [firstPremise] tm ty
                where firstPremise = typeChecking mctx (OCons var fromty octx) body toty
            doTypeChecking mctx octx tm@(Lambda var anno body) ty@(Abstraction fromty toty) = DoesNotCheck [NoDerivation] tm ty

            -- B-BOX
            doTypeChecking mctx _ tm@(Box body) ty@(Boxed bty) = 
                case firstPremise of
                    TypeChecks _ _ subtype | subtype == bty -> TypeChecks [firstPremise] tm ty
                    _ -> DoesNotCheck [firstPremise] tm ty
                where firstPremise = typeChecking mctx OEmpty body bty

            -- B-CHECKING-LETBOX
            doTypeChecking mctx octx tm@(LetBox var ofBody inBody) ty = 
                case firstPremise of
                    TypeSynthesises _ _ (Boxed argType) ->
                        case secondPremise of
                            TypeChecks _ _ bodyType | bodyType == ty -> TypeChecks [firstPremise, secondPremise] tm ty
                            _ -> DoesNotCheck [firstPremise, secondPremise] tm ty
                        where secondPremise = typeChecking (MCons var argType mctx) octx inBody ty
                    _ -> DoesNotCheck [firstPremise, NoDerivation] tm ty
                where firstPremise = typeSynthesising mctx octx ofBody

            -- B-PAIR
            doTypeChecking mctx octx tm@(Pair left right) ty@(Product lty rty) = 
                case firstPremise of
                    TypeChecks _ _ fty | fty == lty -> 
                        case secondPremise of
                            TypeChecks _ _ sty | sty == rty -> TypeChecks [firstPremise, secondPremise] tm ty
                            _ -> DoesNotCheck [firstPremise, secondPremise] tm ty
                        where secondPremise = typeChecking mctx octx right rty
                    _ -> DoesNotCheck [firstPremise, NoDerivation] tm ty
                where firstPremise = typeChecking mctx octx left lty
                        

            -- B-UNIT
            doTypeChecking _ _ EmptyPair Unit = TypeChecks [] EmptyPair Unit
            -- B-Z
            doTypeChecking _ _ Zero Natural = TypeChecks [] Zero Natural
            -- B-S
            doTypeChecking mctx octx tm@(Succ body) ty@Natural = 
                case premise of
                    TypeChecks _ _ bty | bty == Natural -> TypeChecks [premise] tm ty
                    _ -> DoesNotCheck [premise] tm ty
                where premise = typeChecking mctx octx body Natural
            -- B-CHECKING-CASE
            doTypeChecking mctx octx tm@(Case depend zero var succ) ty = 
                case firstPremise of
                    TypeChecks _ _ _ ->
                        case secondPremise of
                            TypeChecks _ _ _ ->
                                case thirdPremise of
                                    TypeChecks _ _ _ -> TypeChecks [firstPremise, secondPremise, thirdPremise] tm ty
                                    _ -> DoesNotCheck [firstPremise, secondPremise, thirdPremise] tm ty
                                where thirdPremise = typeChecking mctx (OCons var Natural octx) succ ty
                            _ -> DoesNotCheck [firstPremise, secondPremise, NoDerivation] tm ty
                        where secondPremise = typeChecking mctx octx zero ty
                    _ -> DoesNotCheck [firstPremise, NoDerivation, NoDerivation] tm ty
                where firstPremise = typeChecking mctx octx depend Natural
            -- B-CHANGE-DIR
            doTypeChecking mctx octx body ty = 
                case premise of
                    TypeSynthesises _ _ sty | sty == ty -> TypeChecks [premise] body ty
                    _ -> DoesNotCheck [premise] body ty
                where premise = typeSynthesising mctx octx body

            doTypeChecking _ _ tm ty = DoesNotCheck [] tm ty

typeSynthesising :: ModalContext -> OrdinaryContext -> Term -> Derivation
-- B-OVAR
typeSynthesising _ octx tm@(Var var) = 
    case fromOrdinaryContext var octx of
        Just ty -> TypeSynthesises [] tm ty
        Nothing -> DoesNotSynthesise [] tm
-- B-APP
typeSynthesising mctx octx tm@(Application abs arg) = 
    case firstPremise of
        TypeSynthesises _ _ (Abstraction from to) -> 
            case secondPremise of
                TypeChecks _ _ _ -> TypeSynthesises [firstPremise, secondPremise] tm to
                _ -> DoesNotSynthesise [firstPremise, secondPremise] tm
            where secondPremise = typeChecking mctx octx arg from
        _ -> DoesNotSynthesise [firstPremise, NoDerivation] tm
    where firstPremise = typeSynthesising mctx octx abs
-- B-MVAR
typeSynthesising mctx _ tm@(ModalVar mvar) = 
    case fromModalContext mvar mctx of
        Just ty -> TypeSynthesises [] tm ty
        Nothing -> DoesNotSynthesise [] tm
-- B-SYNTHESISING-LETBOX
typeSynthesising mctx octx tm@(LetBox var ofBody inBody) = 
    case firstPremise of
        TypeSynthesises _ _ ofty -> 
            case secondPremise of
                TypeSynthesises _ _ bodyty -> TypeSynthesises [firstPremise, secondPremise] tm bodyty
                _ -> DoesNotSynthesise [firstPremise, secondPremise] tm
            where secondPremise = typeSynthesising (MCons var ofty mctx) octx inBody
        _ -> DoesNotSynthesise [firstPremise, NoDerivation] tm
    where firstPremise = typeSynthesising mctx octx ofBody
-- B-FST
typeSynthesising mctx octx tm@(First body) = 
    case premise of
        TypeSynthesises _ _ (Product lty _) -> TypeSynthesises [premise] tm lty
        _ -> DoesNotSynthesise [premise] tm
    where premise = typeSynthesising mctx octx body
-- B-SND
typeSynthesising mctx octx tm@(Second body) = 
    case premise of
        TypeSynthesises _ _ (Product _ rty) -> TypeSynthesises [premise] tm rty
        _ -> DoesNotSynthesise [premise] tm
    where premise = typeSynthesising mctx octx body
-- B-SYNTHESISING-CASE
typeSynthesising mctx octx tm@(Case depend zero var succ) = 
    case firstPremise of
        TypeChecks _ _ dependty | dependty == Natural ->
            case secondPremise of
                TypeSynthesises _ _ zeroty ->
                    case thirdPremise of
                        TypeSynthesises _ _ succty | zeroty == succty -> 
                            TypeSynthesises [firstPremise, secondPremise, thirdPremise] tm succty
                        _ -> DoesNotSynthesise [firstPremise, secondPremise, thirdPremise] tm
                    where thirdPremise = typeSynthesising mctx (OCons var Natural octx) succ
                _ -> DoesNotSynthesise [firstPremise, secondPremise, NoDerivation] tm
            where secondPremise = typeSynthesising mctx octx zero
        _ -> DoesNotSynthesise [firstPremise, NoDerivation, NoDerivation] tm
    where firstPremise = typeChecking mctx octx depend Natural
-- B-FIX TODO := Make Type Checking
typeSynthesising mctx octx tm@(Fix var anno body) = 
    case premise of
        TypeChecks _ _ ty | anno == ty -> TypeSynthesises [premise] tm ty
        _ -> DoesNotCheck [premise] tm anno
    where premise = typeChecking mctx (OCons var anno octx) body anno
-- B-ANNO
typeSynthesising mctx octx tm@(Anno expr ty) = 
    case premise of
        TypeChecks _ _ ety | ety == ty -> TypeSynthesises [premise] tm ty
        _ -> DoesNotCheck [premise] tm ty
    where premise = typeChecking mctx octx expr ty

typeSynthesising _ _ tm = DoesNotSynthesise [] tm
