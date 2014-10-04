(** AST_TRANSFORMATIONS.ML
    Implementation of some simple program transformations
    that occur at the AST level
*)

open Abstract_syntax_tree


(** Apply the local transformation [statf] to each 
    statement of a program *)

let transform_prog statf (toplevel, ext) = 

  let rec transform_stat (stat, ext) = 

    let stat' = set_ext ext (
      match stat with 
      | AST_block b -> AST_block (List.map transform_stat b)
      | s -> s
    ) 
    in statf stat'
  
  and transform_toplevel_item = function
    | AST_stat s -> AST_stat (transform_stat s)
    | t -> t

  in (List.map transform_toplevel_item toplevel, ext)



(** Phantom variables for loops *)


(* Generates fresh names for phantom variables *)

let phantoms_counter = ref 0
let clear_phantoms_counter () = 
  phantoms_counter := 0

let fresh_phantom () = 
  incr phantoms_counter;
  Printf.sprintf "%%%d" !phantoms_counter

    
let add_phantom_incr phantom ext_stat = AST_block [
  ext_stat ;
  dmy_ext 
    (AST_assign 
       (dmy_ext phantom, dmy_ext 
         (AST_binary (AST_PLUS, 
                      dmy_ext (AST_identifier (dmy_ext phantom)), 
                      dmy_ext (AST_int_const (dmy_ext "1")))
         ))
    )]

let add_phantom_init phantom ext_stat = AST_block [
  dmy_ext
    (AST_local
       (dmy_ext AST_TYP_INT, 
        [(dmy_ext phantom, Some (
          dmy_ext (AST_int_const (dmy_ext "0")))) ]
       ));
  ext_stat
]

let set_while_phantoms (stat, ext) = match stat with
  | AST_while (cond, body) ->
    let v = fresh_phantom () in
    set_ext ext 
      (add_phantom_init v (set_ext ext (
        
        AST_while (cond, 
                   dmy_ext (add_phantom_incr v body))
       )))

  | _ -> (stat, ext)


let add_loop_phantoms prog = 
  transform_prog set_while_phantoms prog
