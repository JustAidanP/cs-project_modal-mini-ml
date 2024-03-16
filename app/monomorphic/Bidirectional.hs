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
            TypeChecks _ _ _ _ -> check
            -- We couldn't tyoe check, so we do B-CHANGE-DIR, i.e. we try to syntehsise
            _ -> let synth = typeSynthesising inMctx inOctx inTm in
                    case synth of
                        TypeSynthesises _ _ _ sty | sty == inTy -> TypeChecks [synth] (inMctx, inOctx) inTm inTy
                        -- TODO Possible an an error message that we couldn't synth a type either?
                        _ -> check

    where   doTypeChecking :: ModalContext -> OrdinaryContext -> Term -> Type -> Derivation
            -- B-LAM
            doTypeChecking mctx octx tm@(Lambda var anno body) ty@(Abstraction fromty toty) | anno == fromty = 
                case firstPremise of
                    TypeChecks _ _ _ subtype -> TypeChecks [firstPremise] (mctx, octx) tm ty
                    _ -> DoesNotCheck [firstPremise] (mctx, octx) tm ty
                where firstPremise = typeChecking mctx (OCons var fromty octx) body toty
            doTypeChecking mctx octx tm@(Lambda var anno body) ty@(Abstraction fromty toty) = DoesNotCheck [NoDerivation] (mctx, octx) tm ty

            -- B-BOX
            doTypeChecking mctx octx tm@(Box body) ty@(Boxed bty) = 
                case firstPremise of
                    TypeChecks _ _ _ subtype | subtype == bty -> TypeChecks [firstPremise] (mctx, octx) tm ty
                    _ -> DoesNotCheck [firstPremise] (mctx, octx) tm ty
                where firstPremise = typeChecking mctx OEmpty body bty

            -- B-CHECKING-LETBOX
            doTypeChecking mctx octx tm@(LetBox var ofBody inBody) ty = 
                case firstPremise of
                    TypeSynthesises _ _ _ (Boxed argType) ->
                        case secondPremise of
                            TypeChecks _ _ _ bodyType | bodyType == ty -> TypeChecks [firstPremise, secondPremise] (mctx, octx) tm ty
                            _ -> DoesNotCheck [firstPremise, secondPremise] (mctx, octx) tm ty
                        where secondPremise = typeChecking (MCons var argType mctx) octx inBody ty
                    _ -> DoesNotCheck [firstPremise, NoDerivation] (mctx, octx) tm ty
                where firstPremise = typeSynthesising mctx octx ofBody

            -- B-PAIR
            doTypeChecking mctx octx tm@(Pair left right) ty@(Product lty rty) = 
                case firstPremise of
                    TypeChecks _ _ _ fty | fty == lty -> 
                        case secondPremise of
                            TypeChecks _ _ _ sty | sty == rty -> TypeChecks [firstPremise, secondPremise] (mctx, octx) tm ty
                            _ -> DoesNotCheck [firstPremise, secondPremise] (mctx, octx) tm ty
                        where secondPremise = typeChecking mctx octx right rty
                    _ -> DoesNotCheck [firstPremise, NoDerivation] (mctx, octx) tm ty
                where firstPremise = typeChecking mctx octx left lty
                        

            -- B-UNIT
            doTypeChecking mctx octx EmptyPair Unit = TypeChecks [] (mctx, octx) EmptyPair Unit
            -- B-Z
            doTypeChecking mctx octx Zero Natural = TypeChecks [] (mctx, octx) Zero Natural
            -- B-S
            doTypeChecking mctx octx tm@(Succ body) ty@Natural = 
                case premise of
                    TypeChecks _ _ _ bty | bty == Natural -> TypeChecks [premise] (mctx, octx) tm ty
                    _ -> DoesNotCheck [premise] (mctx, octx) tm ty
                where premise = typeChecking mctx octx body Natural
            -- B-CHECKING-CASE
            doTypeChecking mctx octx tm@(Case depend zero var succ) ty = 
                case firstPremise of
                    TypeChecks _ _ _ _ ->
                        case secondPremise of
                            TypeChecks _ _ _ _ ->
                                case thirdPremise of
                                    TypeChecks _ _ _ _ -> TypeChecks [firstPremise, secondPremise, thirdPremise] (mctx, octx) tm ty
                                    _ -> DoesNotCheck [firstPremise, secondPremise, thirdPremise] (mctx, octx) tm ty
                                where thirdPremise = typeChecking mctx (OCons var Natural octx) succ ty
                            _ -> DoesNotCheck [firstPremise, secondPremise, NoDerivation] (mctx, octx) tm ty
                        where secondPremise = typeChecking mctx octx zero ty
                    _ -> DoesNotCheck [firstPremise, NoDerivation, NoDerivation] (mctx, octx) tm ty
                where firstPremise = typeChecking mctx octx depend Natural
            -- B-FIX TODO := Make Type Checking
            doTypeChecking mctx octx tm@(Fix var anno body) ty = 
                case premise of
                    TypeChecks _ _ _ cty | anno == ty -> TypeChecks [premise] (mctx, octx) tm ty
                    _ -> DoesNotCheck [premise] (mctx, octx) tm anno
                where premise = typeChecking mctx (OCons var anno octx) body anno
            -- B-CHANGE-DIR
            doTypeChecking mctx octx body ty = 
                case premise of
                    TypeSynthesises _ _ _ sty | sty == ty -> TypeChecks [premise] (mctx, octx) body ty
                    _ -> DoesNotCheck [premise] (mctx, octx) body ty
                where premise = typeSynthesising mctx octx body

            doTypeChecking mctx octx tm ty = DoesNotCheck [] (mctx, octx) tm ty

typeSynthesising :: ModalContext -> OrdinaryContext -> Term -> Derivation
-- B-OVAR
typeSynthesising mctx octx tm@(Var var) = 
    case fromOrdinaryContext var octx of
        Just ty -> TypeSynthesises [] (mctx, octx) tm ty
        Nothing -> DoesNotSynthesise [] (mctx, octx) tm
-- B-APP
typeSynthesising mctx octx tm@(Application abs arg) = 
    case firstPremise of
        TypeSynthesises _ _ _ (Abstraction from to) -> 
            case secondPremise of
                TypeChecks _ _ _ _ -> TypeSynthesises [firstPremise, secondPremise] (mctx, octx) tm to
                _ -> DoesNotSynthesise [firstPremise, secondPremise] (mctx, octx) tm
            where secondPremise = typeChecking mctx octx arg from
        _ -> DoesNotSynthesise [firstPremise, NoDerivation] (mctx, octx) tm
    where firstPremise = typeSynthesising mctx octx abs
-- B-MVAR
typeSynthesising mctx octx tm@(ModalVar mvar) = 
    case fromModalContext mvar mctx of
        Just ty -> TypeSynthesises [] (mctx, octx) tm ty
        Nothing -> DoesNotSynthesise [] (mctx, octx) tm
-- B-SYNTHESISING-LETBOX
typeSynthesising mctx octx tm@(LetBox var ofBody inBody) = 
    case firstPremise of
        TypeSynthesises _ _ _ (Boxed ofty) -> 
            case secondPremise of
                TypeSynthesises _ _ _ bodyty -> TypeSynthesises [firstPremise, secondPremise] (mctx, octx) tm bodyty
                _ -> DoesNotSynthesise [firstPremise, secondPremise] (mctx, octx) tm
            where secondPremise = typeSynthesising (MCons var ofty mctx) octx inBody
        _ -> DoesNotSynthesise [firstPremise, NoDerivation] (mctx, octx) tm
    where firstPremise = typeSynthesising mctx octx ofBody
-- B-FST
typeSynthesising mctx octx tm@(First body) = 
    case premise of
        TypeSynthesises _ _ _ (Product lty _) -> TypeSynthesises [premise] (mctx, octx) tm lty
        _ -> DoesNotSynthesise [premise] (mctx, octx) tm
    where premise = typeSynthesising mctx octx body
-- B-SND
typeSynthesising mctx octx tm@(Second body) = 
    case premise of
        TypeSynthesises _ _ _ (Product _ rty) -> TypeSynthesises [premise] (mctx, octx) tm rty
        _ -> DoesNotSynthesise [premise] (mctx, octx) tm
    where premise = typeSynthesising mctx octx body
-- B-SYNTHESISING-CASE
typeSynthesising mctx octx tm@(Case depend zero var succ) = 
    case firstPremise of
        TypeChecks _ _ _ dependty | dependty == Natural ->
            case secondPremise of
                TypeSynthesises _ _ _ zeroty ->
                    case thirdPremise of
                        TypeSynthesises _ _ _ succty | zeroty == succty -> 
                            TypeSynthesises [firstPremise, secondPremise, thirdPremise] (mctx, octx) tm succty
                        _ -> DoesNotSynthesise [firstPremise, secondPremise, thirdPremise] (mctx, octx) tm
                    where thirdPremise = typeSynthesising mctx (OCons var Natural octx) succ
                _ -> DoesNotSynthesise [firstPremise, secondPremise, NoDerivation] (mctx, octx) tm
            where secondPremise = typeSynthesising mctx octx zero
        _ -> DoesNotSynthesise [firstPremise, NoDerivation, NoDerivation] (mctx, octx) tm
    where firstPremise = typeChecking mctx octx depend Natural
-- B-ANNO
typeSynthesising mctx octx tm@(Anno expr ty) = 
    case premise of
        TypeChecks _ _ _ ety | ety == ty -> TypeSynthesises [premise] (mctx, octx) tm ty
        _ -> DoesNotCheck [premise] (mctx, octx) tm ty
    where premise = typeChecking mctx octx expr ty

typeSynthesising mctx octx tm = DoesNotSynthesise [] (mctx, octx) tm
