open Combinator

let rewrite c =
  match c with
  | I::p::tl -> p::tl
  | K::p::tl -> p::tl
  | S::p::q::r::tl -> p::q::(Application (q, r))::tl
  | B::p::q::r::tl -> p::(Application (q, r))::tl
  | C::p::q::r::tl -> p::r::q::tl
  | c -> c
