open Beam

type erlang_term =
  | Atom of string

module type ABSTRACT = sig
  type expr
  type pattern
  type guard
  type clause
  val expr_to_abstract: expr -> erlang_term
  val expr_to_pattern: pattern -> erlang_term
  val expr_to_guard: guard -> erlang_term
  val expr_to_clause: clause -> erlang_term
end
