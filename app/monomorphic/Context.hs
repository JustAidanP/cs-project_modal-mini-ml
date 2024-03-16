module Monomorphic.Context where

import Monomorphic.Terms
import Monomorphic.Types

data ModalContext = MCons Variable Type ModalContext | MEmpty

instance Show ModalContext where
    show (MCons var ty ctx) = var ++ ":" ++ show ty ++ ", " ++ show ctx
    show MEmpty = "·"

latexPrintMCtx :: ModalContext -> String
latexPrintMCtx (MCons var ty ctx) = "\\underline{" ++ var ++ "}" ++ "\\mathbin{:}" ++ latexPrintTy ty ++ "," ++ latexPrintMCtx ctx
latexPrintMCtx MEmpty = "\\cdot"


fromModalContext :: Variable -> ModalContext -> Maybe Type
fromModalContext var (MCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromModalContext var (MCons _ _ mctx)                       = fromModalContext var mctx
fromModalContext _ MEmpty                                   = Nothing

data OrdinaryContext = OCons Variable Type OrdinaryContext | OEmpty
instance Show OrdinaryContext where
    show (OCons var ty ctx) = var ++ ":" ++ show ty ++ ", " ++ show ctx
    show OEmpty = "·"

latexPrintOCtx :: OrdinaryContext -> String
latexPrintOCtx (OCons var ty ctx) = "\\underline{" ++ var ++ "}" ++ "\\mathbin{:}" ++ latexPrintTy ty ++ "," ++ latexPrintOCtx ctx
latexPrintOCtx OEmpty = "\\cdot"

fromOrdinaryContext :: Variable -> OrdinaryContext -> Maybe Type
fromOrdinaryContext var (OCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromOrdinaryContext var (OCons _ _ octx)                       = fromOrdinaryContext var octx
fromOrdinaryContext _ OEmpty                                   = Nothing