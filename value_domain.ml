(** VALUE_DOMAIN.ML
    An interface for value domains, used for
    non relationnal environment domains
*)

module type S = sig

  type t

  val top : t
  val bottom : t
  val const : int -> t
  val rand : int -> int -> t

  val is_bottom : t -> bool
  val is_top : t -> bool

  val subset : t -> t -> bool

  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t


  (** [assume_leq a b] returns [a', b'] where 
      a' = {x in a | exists y in b : x <= y}
      b' = {y in b | exists x in a : x <= y}

      Intuitively, (u, v) is (a, b) after learning from 
      the assertion a <= b  *)

  val assume_leq : t -> t -> t * t
  val assume_gt : t -> t -> t * t

  val print : t -> string

end
