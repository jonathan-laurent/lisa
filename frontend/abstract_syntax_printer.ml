(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2014
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
  Pretty-printer for abstract syntax trees.
*)

open Abstract_syntax_tree
open Lexing


(* locations *)
(* ********* *)

let string_of_position p =
  Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
    
let string_of_extent (p,q) =
  if p.pos_fname = q.pos_fname then
    if p.pos_lnum = q.pos_lnum then
      if p.pos_cnum = q.pos_cnum then
        Printf.sprintf "%s:%i.%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
      else
        Printf.sprintf "%s:%i.%i-%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) (q.pos_cnum - q.pos_bol)
    else
      Printf.sprintf "%s:%i.%i-%i.%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) q.pos_lnum (q.pos_cnum - q.pos_bol)
  else
    Printf.sprintf "%s:%i.%i-%s:%i.%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol) q.pos_fname q.pos_lnum (q.pos_cnum - q.pos_bol)



(* operators *)
(* ********* *)

let string_of_unary_op = function
  | AST_UNARY_PLUS -> "+"
  | AST_UNARY_MINUS -> "-"
  | AST_NOT -> "!"

let string_of_binary_op = function
  | AST_MULTIPLY -> "*"
  | AST_DIVIDE -> "/"
  | AST_MODULO -> "%"
  | AST_PLUS -> "+"
  | AST_MINUS -> "-"
  | AST_EQUAL -> "=="
  | AST_NOT_EQUAL -> "!="
  | AST_LESS -> "<"
  | AST_LESS_EQUAL -> "<="
  | AST_GREATER -> ">"
  | AST_GREATER_EQUAL -> ">="
  | AST_AND -> "&&"
  | AST_OR -> "||"



(* higher values mean higher precedence *)
let binary_precedence = function
  | AST_MULTIPLY | AST_DIVIDE | AST_MODULO -> 6
  | AST_PLUS  | AST_MINUS -> 5
  | AST_EQUAL | AST_NOT_EQUAL -> 4
  | AST_LESS | AST_LESS_EQUAL | AST_GREATER | AST_GREATER_EQUAL -> 3
  | AST_AND -> 2
  | AST_OR -> 1

(* precedence of the operator at the root of the expression;
   this is used to avoid printing unnecessary parentheses
 *)
let expr_precedence = function
  | AST_unary (op, _) -> 99
  | AST_binary(op, _, _) -> binary_precedence op
  | _ -> 100


(* utility to print lists *)
let print_list f sep fmt l =
  let rec aux = function
    | [] -> ()
    | [a] -> f fmt a
    | a::b -> f fmt a; Format.pp_print_string fmt sep; aux b
  in
  aux l


(* types *)
(* ***** *)

let string_of_typ = function
  | AST_TYP_INT -> "int"
  | AST_TYP_BOOL -> "bool"
  | AST_TYP_AUTO -> "auto"


(* expressions *)
(* *********** *)

let print_id fmt v =
  Format.pp_print_string fmt v

let rec print_expr fmt e = 
  match e with
    
  | AST_unary (op,(e1,_)) ->
      Format.pp_print_string fmt (string_of_unary_op op);
      if expr_precedence e1 <= expr_precedence e
      then Format.fprintf fmt " (%a)" print_expr e1
      else Format.fprintf fmt " %a" print_expr e1

  | AST_binary (op,(e1,_),(e2,_)) ->
      if expr_precedence e1 < expr_precedence e
      then Format.fprintf fmt "(%a) " print_expr e1
      else Format.fprintf fmt "%a " print_expr e1;
      Format.pp_print_string fmt (string_of_binary_op op);
      if expr_precedence e2 <= expr_precedence e
      then Format.fprintf fmt " (%a)" print_expr e2
      else Format.fprintf fmt " %a" print_expr e2
          
  | AST_int_const (i,_) -> Format.pp_print_string fmt i

  | AST_int_rand ((i1,_),(i2,_)) -> Format.fprintf fmt "rand(%s, %s)" i1 i2

  | AST_bool_const b -> Format.pp_print_bool fmt b
        
  | AST_identifier (v,_) -> print_id fmt v

  | AST_expr_call ((i,_),l) ->
      Format.fprintf fmt "%a(%a)"
        print_id i (print_list print_expr ",") (List.map fst l)

let print_lvalue fmt v =
  Format.pp_print_string fmt v



(* statements *)
(* ********** *)

let indent ind = ind^"  "

(* ind is a string of spaces (indentation) to put at the begining of each line
 *)
let rec print_stat ind fmt = function

  | AST_block b ->
      print_block ind fmt b

  | AST_assign ((v,_),(e,_)) ->
      Format.fprintf fmt "%s%a = %a;@\n" 
        ind print_lvalue v print_expr e

  | AST_if ((e,_), (b1,_), None) ->
      Format.fprintf fmt "%sif (%a)@\n%a" 
        ind print_expr e (print_stat (indent ind)) b1

  | AST_if ((e,_), (b1,_), Some (b2,_)) ->
      Format.fprintf fmt "%sif (%a)@\n%a%selse@\n%a" 
        ind print_expr e (print_stat (indent ind)) b1
        ind (print_stat (indent ind)) b2

  | AST_while ((e,_),(b,_)) ->
      Format.fprintf fmt "%swhile (%a)@\n%a" 
        ind print_expr e (print_stat (indent ind)) b

  | AST_assert ((e,_)) ->
      Format.fprintf fmt "%sassert (%a);@\n" 
        ind print_expr e 

  | AST_print l ->
      Format.fprintf fmt "%sprint (%a);@\n" 
        ind (print_list print_id ",") (List.map fst l)

  | AST_local d ->
      Format.fprintf fmt "%s%a" ind print_var_decl d

  | AST_stat_call ((i,_),l) ->
      Format.fprintf fmt "%s%a(%a);@\n"
        ind print_id i (print_list print_expr ",") (List.map fst l)
 
  | AST_return None ->
      Format.fprintf fmt "%sreturn;@\n" ind

  | AST_return (Some (e,_)) ->
      Format.fprintf fmt "%sreturn %a;@\n" 
        ind print_expr e

  | AST_BREAK ->
      Format.fprintf fmt "%sbreak;@\n" ind

  | AST_HALT ->
      Format.fprintf fmt "%shalt;@\n" ind

  | AST_label (l,_) ->
      Format.fprintf fmt "%s%a:@\n" ind print_id l

  | AST_goto (l,_) ->
      Format.fprintf fmt "%sgoto %a;@\n" ind print_id l

and print_block ind fmt b =
  Format.fprintf fmt "%s{@\n" ind;
  List.iter (fun (bb,_) -> print_stat (indent ind) fmt bb) b;
  Format.fprintf fmt "%s}@\n" ind



(* declarations *)
(* ************ *)

and print_var_decl fmt ((t,_),l) =
  Format.fprintf fmt "%s %a;@\n" 
    (string_of_typ t) (print_list print_var_init ", ") l

and print_var_init fmt ((i,_),eo) =
  print_id fmt i;
  match eo with
  | None -> ()
  | Some (e,_) -> Format.fprintf fmt " = %a" print_expr e

let print_arg_decl fmt ((t,_),(i,_)) =
  Format.fprintf fmt "%s %a"
    (string_of_typ t) print_id i

let print_fun_decl fmt f =
  Format.fprintf fmt "%s %a(%a)@\n%a"
    (match fst (f.fun_typ) with None -> "void" | Some t -> string_of_typ t)
    print_id (fst f.fun_name)
    (print_list print_arg_decl  ", ") f.fun_args
    (print_block "") f.fun_body

let print_toplevel fmt = function
  | AST_stat (s,_) -> print_stat "" fmt s
  | AST_fun_decl (f,_) -> print_fun_decl fmt f


(* programs *)
(* ******** *)

let print_prog fmt (p,_) =
  List.iter (print_toplevel fmt) p
