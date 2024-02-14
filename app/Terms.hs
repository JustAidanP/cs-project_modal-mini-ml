module Terms where

import Types

data Term = Var Variable 
            | Lambda { variable :: Variable, variable_annotation :: Type, body :: Term } 
            | Application { lambda :: Term, argument :: Term }
            | ModalVar Variable
            | Box Term
            | LetBox { modalVariable :: Variable, box :: Term, body :: Term }
            | Pair { left :: Term, right :: Term } 
            | First Term
            | Second Term
            | EmptyProduct
            | Zero
            | Successor Term
            | Case { dependent :: Term, zero :: Term, successor :: Term }
            | Fix { variable :: Variable, annotation :: Type, body :: Term }