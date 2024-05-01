module Polymorphic.Context where

import Data.List

import Polymorphic.Terms
import Polymorphic.Types

data ModalContext = MCons Variable PolyType ModalContext | MEmpty

instance Show ModalContext where
    show (MCons var ty ctx) = var ++ ":" ++ show ty ++ ", " ++ show ctx
    show MEmpty = "·"

latexPrintMCtx :: ModalContext -> String
latexPrintMCtx (MCons var ty ctx) = "\\underline{" ++ var ++ "}" ++ "\\mathbin{:}" ++ latexPrintPolyTy ty ++ "," ++ latexPrintMCtx ctx
latexPrintMCtx MEmpty = "\\cdot"

fromModalContext :: Variable -> ModalContext -> Maybe PolyType
fromModalContext var (MCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromModalContext var (MCons _ _ mctx)                       = fromModalContext var mctx
fromModalContext _ MEmpty                                   = Nothing

freeVariablesModalCtx :: ModalContext -> [Variable]
freeVariablesModalCtx (MEmpty) = []
freeVariablesModalCtx (MCons _ ty mctx) = nub ((freeVariablesPolyType ty) ++ (freeVariablesModalCtx mctx))

data OrdinaryContext = OCons Variable MonoType OrdinaryContext | OEmpty
instance Show OrdinaryContext where
    show (OCons var ty ctx) = var ++ ":" ++ show ty ++ ", " ++ show ctx
    show OEmpty = "·"

latexPrintOCtx :: OrdinaryContext -> String
latexPrintOCtx (OCons var ty ctx) = "\\underline{" ++ var ++ "}" ++ "\\mathbin{:}" ++ latexPrintMonoTy ty ++ "," ++ latexPrintOCtx ctx
latexPrintOCtx OEmpty = "\\cdot"

fromOrdinaryContext :: Variable -> OrdinaryContext -> Maybe MonoType
fromOrdinaryContext var (OCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromOrdinaryContext var (OCons _ _ octx)                       = fromOrdinaryContext var octx
fromOrdinaryContext _ OEmpty                                   = Nothing

freeVariablesOrdinaryCtx :: OrdinaryContext -> [Variable]
freeVariablesOrdinaryCtx (OEmpty) = []
freeVariablesOrdinaryCtx (OCons _ ty octx) = nub ((freeVariablesMonoType ty) ++ (freeVariablesOrdinaryCtx octx))