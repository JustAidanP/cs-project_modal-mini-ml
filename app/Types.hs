module Types where

type Variable = String
data Type = Natural | Function Type Type | Product Type Type | Unit | Boxed Type
