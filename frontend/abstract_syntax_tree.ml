(*
  Cours "Sémantique et Application à la Vérification de programmes"
  
  Antoine Miné 2014
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(* 
  Definition of the abstract syntax trees output by the parser.
*)


open Lexing


(* position in the source file, we use ocamllex's default type *)

type position = Lexing.position
let position_unknown = Lexing.dummy_pos

let print_one_line_extent (pos1, pos2) = 
  Lexing.(
    Printf.sprintf "%s:%d" 
      pos1.pos_fname pos1.pos_lnum 
  )


(* extents are pairs of positions *)

type extent = position * position (* start/end *)
let extent_unknown = (position_unknown, position_unknown)
let rm_ext (e, _) = e
let set_ext ext v = (v, ext)
let dmy_ext e = (e, extent_unknown)



(* Many parts of the syntax are tagged with an extent indicating which 
   part of the parser-file corresponds to the sub-tree.
   This is very useful for interesting error reporting!
 *)
type 'a ext = 'a * extent

(* variable identifiers, just strings for now *)
(* local variables and scoping would require using UNIQUE IDENTIFIERS
   to handle the case where several variables have the same name
 *)
type id = string

(* types in our language *)
type typ =
  (* mathematical integers, in Z *)
  | AST_TYP_INT

  (* booleans: true or false *)
  | AST_TYP_BOOL
     
  (* variables declared with the auto type should have their type
     inferred automatically
   *)
  | AST_TYP_AUTO


(* unary expression operators *)
type  unary_op = 

  (* arithmetic operators *)
  | AST_UNARY_PLUS     (* +e *)
  | AST_UNARY_MINUS    (* -e *)

  (* logical operators *)
  | AST_NOT            (* !e logical negation *)


(* binary expression operators *)
type binary_op =

  (* arithmetic operators, work only for int *)
  | AST_PLUS          (* e + e *)
  | AST_MINUS         (* e - e *)
  | AST_MULTIPLY      (* e * e *)
  | AST_DIVIDE        (* e / e *)
  | AST_MODULO        (* e % e *)
  
  (* polymorphic comparison, should work for int and bool *)
  | AST_EQUAL         (* e == e *)
  | AST_NOT_EQUAL     (* e != e *)

  (* arithmetic comparisons, work only for int *)
  | AST_LESS          (* e < e *)
  | AST_LESS_EQUAL    (* e <= e *)
  | AST_GREATER       (* e > e *)
  | AST_GREATER_EQUAL (* e >= e *)

  (* boolean operators, work only for bool *)
  | AST_AND           (* e && e *)
  | AST_OR            (* e || e *)


(* expressions *)
type expr = 
  (* unary operation *)
  | AST_unary of unary_op * (expr ext)

  (* binary operation *)
  | AST_binary of binary_op * (expr ext) * (expr ext)

  (* variable use *)
  | AST_identifier of id ext

  (* constants (integers are still in their string representation) *)
  | AST_int_const of string ext
  | AST_bool_const of bool

  (* non-deterministic choice between two integeres *)
  | AST_int_rand of (string ext) (* lower bound *) * 
                    (string ext) (* upper bound *)

  (* EXTENSIONS *)
  (* support for them is OPTIONAL *)

  (* calls a function with arguments and return value *)
  | AST_expr_call of (id ext) (* function name *) * 
                     (expr ext list) (* arguments *)


(* left part of assignments *)
type lvalue = id

(* statements *)
type stat =

  (* block of statements { ... } *)
  | AST_block of stat ext list

  (* assignment  lvalue = expr *)
  | AST_assign of (lvalue ext) * (expr ext)

  (* if-then-else; the else branch is optional *)
  | AST_if of (expr ext) (* condition *) * 
              (stat ext) (* then branch *) * 
              (stat ext option) (* optional else *)

  (* while loop *)
  | AST_while of (expr ext) (* condition *) * 
                 (stat ext) (* body *)

  (* exits the program *)
  | AST_HALT

  (* assertion: fail if the boolean expression does not hold *)
  | AST_assert of expr ext

  (* prints the value of some variables *)
  | AST_print of (lvalue ext) list


  (* EXTENSIONS *)
  (* support for them is OPTIONAL *)

  (* declaration of a local variable, live until the end of the current block *)
  | AST_local of var_decl

  (* calls a function with arguments (no return value) *)
  | AST_stat_call of (id ext) (* function name *) * 
                     (expr ext list) (* arguments *)

  (* exits form the function, with optional return value *)
  | AST_return of expr ext option

  (* exits from the innermost while loop *)
  | AST_BREAK

  (* defines the position of goto target *)
  | AST_label of id ext

  (* jump to a goto label *)
  | AST_goto of id ext


(* supporting local declarations is OPTIONAL *)

(* declare some variables with a common type *)
and var_decl = (typ ext) (* type *) * (var_init list) (* variable list *)

(* each declared variable has an optional initializer *)
and var_init = (id ext) (* declared variable *) * 
               (expr ext option)  (* initializer *)


(* supporting functions is OPTIONAL *)

(* function declaration 
   (no return type, all functions return void)
 *)
type fun_decl = 
    { (* function name *)
      fun_name: id ext;

      (* formal arguments, with type *)
      fun_args: ((typ ext) * (id ext)) list;

      (* type of the returned value, if any *)
      fun_typ: typ option ext;

      (* function body *)
      fun_body: stat ext list;   
    }


(* top-level statements *)
type toplevel =

  (* statement to execute & variable declarations *)
  | AST_stat of stat ext

  (* function declaration *)
  | AST_fun_decl of fun_decl ext


(* a program is a list of top-level statements *)
type prog = toplevel list ext
