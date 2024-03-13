module Monomorphic.Context where

import Monomorphic.Terms
import Monomorphic.Types

data ModalContext = MCons Variable Type ModalContext | MEmpty

fromModalContext :: Variable -> ModalContext -> Maybe Type
fromModalContext var (MCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromModalContext var (MCons _ _ mctx)                       = fromModalContext var mctx
fromModalContext _ MEmpty                                   = Nothing

data OrdinaryContext = OCons Variable Type OrdinaryContext | OEmpty

fromOrdinaryContext :: Variable -> OrdinaryContext -> Maybe Type
fromOrdinaryContext var (OCons ctxVar ctxTy _) | var == ctxVar = Just ctxTy
fromOrdinaryContext var (OCons _ _ octx)                       = fromOrdinaryContext var octx
fromOrdinaryContext _ OEmpty                                   = Nothing