module Types where

type Variable = String
data Type = Natural | Abstraction Type Type | Product Type Type | Unit | Boxed Type