-- We implement the (first) bidirectional rules for the Modal Mini ML [TODO: We need to name this first system]

module Monomorphic.Bidirectional where
import Monomorphic.Types
import Monomorphic.Terms

data ModalContext = MCons Variable Type ModalContext | MEmpty
data OrdinaryContext = OCons Variable Type OrdinaryContext | OEmpty

fromModalContext :: Variable -> ModalContext -> Maybe Type
fromModalContext var (MCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromModalContext var (MCons _ _ mctx)                       =  fromModalContext var mctx
fromModalContext _ MEmpty                                   = Nothing
fromOrdinaryContext :: Variable -> OrdinaryContext -> Maybe Type
fromOrdinaryContext var (OCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromOrdinaryContext var (OCons _ _ octx)                       =  fromOrdinaryContext var octx
fromOrdinaryContext _ OEmpty                                   = Nothing

-- We first implement all of the Type Checking rules
typeChecking :: ModalContext -> OrdinaryContext -> Term -> Type -> Bool
-- B-LAM
typeChecking mctx octx (Lambda var anno body) (Abstraction fromty toty) | anno == fromty = typeChecking mctx (OCons var fromty octx) body toty
typeChecking mctx octx (Lambda var anno body) (Abstraction fromty toty)                  = False
-- B-BOX
typeChecking mctx _ (Box body) (Boxed ty) = typeChecking mctx OEmpty body ty
-- B-CHECKING-LETBOX
typeChecking mctx octx (LetBox var ofBody inBody) ty = case typeSynthesising mctx octx ofBody of
                                                            Just (Boxed sty) -> typeChecking (MCons var sty mctx) octx inBody ty
                                                            _              -> False
-- B-PAIR
typeChecking mctx octx (Pair left right) (Product lty rty) = (typeChecking mctx octx left lty) &&  (typeChecking mctx octx right rty)
-- B-UNIT
typeChecking _ _ EmptyProduct Unit = True
-- B-Z
typeChecking _ _ Zero Natural = True
-- B-S
typeChecking mctx octx (Succ body) Natural = typeChecking mctx octx body Natural
-- B-CHECKING-CASE
typeChecking mctx octx (Case depend zero var succ) ty = (typeChecking mctx octx depend Natural) && (typeChecking mctx octx zero ty) && (typeChecking mctx (OCons var Natural octx) succ ty)
-- B-CHANGE-DIR
typeChecking mctx octx body ty = case typeSynthesising mctx octx body of
                                    Just sty -> ty == sty
                                    _        -> False


typeSynthesising :: ModalContext -> OrdinaryContext -> Term -> Maybe Type
-- B-OVAR
typeSynthesising _ octx (Var var) = case fromOrdinaryContext var octx of
                                        Just ty -> Just ty
                                        Nothing -> Nothing
-- B-APP
typeSynthesising mctx octx (Application abs arg) = case typeSynthesising mctx octx abs of
                                            Just (Abstraction from to) -> if (typeChecking mctx octx arg from) then (Just to) else Nothing
                                            _                           -> Nothing
-- B-MVAR
typeSynthesising mctx _ (ModalVar mvar) = case fromModalContext mvar mctx of
                                        Just ty -> Just ty
                                        Nothing -> Nothing
-- B-SYNTHESISING-LETBOX
typeSynthesising mctx octx (LetBox var ofBody inBody) = case typeSynthesising mctx octx ofBody of
                                                            Just (Boxed ofty) -> typeSynthesising (MCons var ofty mctx) octx inBody
                                                            Nothing -> Nothing
-- B-FST
typeSynthesising mctx octx (First body) = case typeSynthesising mctx octx body of
                                            Just (Product lty _) -> Just lty
                                            Nothing -> Nothing
-- B-SND
typeSynthesising mctx octx (Second body) = case typeSynthesising mctx octx body of
                                            Just (Product _ rty) -> Just rty
                                            Nothing -> Nothing
-- B-SYNTHESISING-CASE
typeSynthesising mctx octx (Case depend zero var succ) = if typeChecking mctx octx depend Natural 
                                                         then case typeSynthesising mctx octx zero of
                                                                    Just zty -> case typeSynthesising mctx (OCons var Natural octx) succ of
                                                                                    Just sty | zty == sty -> Just zty
                                                                                    Nothing               -> Nothing
                                                                    Nothing  -> Nothing
                                                        else Nothing
-- B-FIX
typeSynthesising mctx octx (Fix var anno body) = if (typeChecking mctx (OCons var anno octx) body anno) then (Just anno) else Nothing
-- B-ANNO
typeSynthesising mctx octx (Anno expr ty) = if (typeChecking mctx octx expr ty) then (Just ty) else Nothing
