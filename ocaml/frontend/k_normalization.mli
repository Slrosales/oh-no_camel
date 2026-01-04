module Syntax = Core.Syntax

module IntSet : Set.OrderedType

val remember_var: IntSet.t ref

val normalization_function : Syntax.t -> Syntax.t