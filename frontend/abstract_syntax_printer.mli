(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2014
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
  Pretty-printer for abstract syntax trees.
*)

open Format
open Abstract_syntax_tree

(* locations *)
val string_of_position: position -> string
val string_of_extent: extent -> string


(* printers *)

val string_of_typ: typ -> string

val print_id: formatter -> id -> unit
val print_lvalue: formatter -> lvalue -> unit
val print_expr: formatter -> expr -> unit
val print_stat: string -> formatter -> stat -> unit
val print_block: string -> formatter -> stat ext list -> unit
val print_var_decl: formatter -> var_decl -> unit
val print_fun_decl: formatter -> fun_decl -> unit
val print_prog: formatter -> prog -> unit
