# An implementation of (partial) Type Inference for the Modal Mini ML

### Disclaimer

This repo corresponds to my 4th year Integrated Masters, Mathematics and Computer Science dissertation. 
It is not intended to be used outside of this context.

# Prerequisites

We assume a full installation of Haskell.

## Monomorphic Type Inference

The monomorphic type inference implementation is contained within the `Monomorphic` module.
To use this implementation, add the following imports:

```{haskell}
import Monomorphic.Types
import Monomorphic.Terms
import Monomorphic.Context
import qualified Monomorphic.Simple.Inference
import qualified Monomorphic.Annotated.Inference
```

### Types

Types are defined within the `Monomorphic.Types` sub-module. They are defined closely to their corresponding grammar.

As an example, to construct a type representing a lambda abstraction from a natural number to the unit type, you can write:

```{haskell}
(Abstraction Natural Unit)
```

Or, a pair of identity functions, the left for natural numbers, and the right for units

```{haskell}
(Product (Abstraction Natural Natural) (Abstraction Unit Unit))
```

### Terms

Terms are defined within the `Monomorphic.Terms` sub-module. They are defined closely to their corresponding grammar.

#### Note
Variables are just strings

### Usage

To define the identity for natural numbers function:

```{haskell}
(Lambda "x" Natural (Var "x"))
```

### Contexts

Contexts are defined in the sub-module `Monomorphic.Contexts`. They are defined closely to their corresponding grammar and the cons Haskell operator.

To create a modal context consisting of two variables:

```{haskell}
MCons "v" Unit (MCons "u" Natural MEmpty)
```

### Inference

Two inference functions are provided: one simple inference method, which only returns the type, and an annotated one, which returns an annotation tree.

The first is located in the sub-module `Monomorphic.Simple.Inference`, the second is in `Monomorphic.Annotated.Inference`.

Once one is chosen, you can infer the type of a term, under some contexts as:

```{haskell}
infer MEmpty OEmpty (Anno (Lambda "x" Natural (Var "x")) (Abstraction Natural Natural))
```

### Example Terms

Some example terms are provided in the `Monomorphic` module.

## Polymorphic Type Inference

Similar to the monomorphic inference, except the following imports are required **instead**:

```{haskell}
import Polymorphic.Types
import Polymorphic.Terms
import Polymorphic.Context
import qualified Polymorphic.Simple.Inference
```

Only simple polymorphic inference has been implemented.

### Example Terms

Some example terms are provided in the `Polymorphic` module.