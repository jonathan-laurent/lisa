(** ENVIRONMENT_DOMAIN.ML
    The interface of the domains used by the [Analysis] module
*)


open Abstract_syntax_tree

module type S = sig

  type t

  val init : unit -> t

  val set_bottom : t -> t
  val is_bottom : t -> bool

  val add_variable : t -> id -> t
  val remove_variable : t -> id -> t
  val vars_list : t -> id list

  val assign : t -> id -> expr -> t
  val assume_leq : t -> expr -> expr -> t
  val assume_gt : t -> expr -> expr -> t

  val subset : t -> t -> bool

  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t

  val print : t -> string
 
end

module Lib (E : S) = struct

  open Utils

  let filter_vars vars env = 
    let vars_to_remove =  list_diff (E.vars_list env) vars in
    List.fold_left E.remove_variable env vars_to_remove

  let print_vars vars env =
    E.print (filter_vars vars env)

end

