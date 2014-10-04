(** POLYHEDRA.ML 
    A wrapper of the Apron Polka library for the 
    [Environment_domain] interface
*)

module type PARAMS = sig
  val zero_plane_projection : bool
end

module Standard : PARAMS = struct
  let zero_plane_projection = false
end

module Make (Params : PARAMS) : Environment_domain.S = struct

  open Apron
  open Abstract_syntax_tree
  open Utils

  type man = Polka.loose Polka.t Manager.t
  type env = Environment.t
  type abstr = Polka.loose Polka.t Abstract1.t

  type t = {tman : man ; tenv : env ; tabstr : abstr}

  let rounding_mode = Apron.Texpr1.Down

  let init () = 
    let env     = Environment.make [||] [||]    in
    let man     = Polka.manager_alloc_loose ()  in
    let top     = Abstract1.top man env      in
    {tman = man ; tenv = env ; tabstr = top}

  let set_bottom d = 
    {d with tabstr = Abstract1.bottom d.tman d.tenv}

  let change_env d env' = 
    let abstr' = Abstract1.change_environment 
      d.tman d.tabstr env' 
      Params.zero_plane_projection in
    {d with tenv = env' ; tabstr = abstr'}

  let add_variable d id = change_env d
    (Environment.add d.tenv [|Var.of_string id|] [||]) 

  let remove_variable d id = change_env d
    (Environment.remove d.tenv [|Var.of_string id|])

  let vars_list d =   
    List.map Var.to_string (Array.to_list (fst (Environment.vars d.tenv)))

  let is_bottom d = Abstract1.is_bottom d.tman d.tabstr



  exception Top

  let rec texpr_of_int_expr expr = 
    let open Abstract_syntax_tree in
    let open Texpr1 in
    match expr with
    | AST_unary (AST_UNARY_PLUS, e) -> 
      texpr_of_int_expr (rm_ext e)
    | AST_unary (AST_UNARY_MINUS, e) -> 
      Unop (Neg, texpr_of_int_expr (rm_ext e), Int, rounding_mode)

    | AST_unary (AST_NOT, _) -> assert false

    | AST_binary (op, lhs, rhs) ->
      (
        let ops_map = 
          [(AST_PLUS, Add) ; (AST_MINUS, Sub) ; 
           (AST_MULTIPLY, Mul) ; (AST_DIVIDE, Div) ; 
           (AST_MODULO, Mod)] in

        try
          Binop (List.assoc op ops_map,
                 texpr_of_int_expr (rm_ext lhs),
                 texpr_of_int_expr (rm_ext rhs),
                 Int, rounding_mode )
            
        with Not_found -> assert false
      )

    | AST_identifier id -> Var (Var.of_string (rm_ext id))

    | AST_int_const c -> 
      Cst (Coeff.Scalar 
             (Scalar.of_int (int_of_string (rm_ext c))))

    | AST_int_rand (a, b) ->
      Cst (Coeff.Interval (Interval.of_int
             (int_of_string (rm_ext a)) (int_of_string (rm_ext b)) ))

    | _ -> assert false



  let print d = 
    let ff = Format.str_formatter in
    Abstract1.print ff d.tabstr;
    Format.flush_str_formatter ()


  let assign d id expr =
    let abstr' = 
      try
        let e = texpr_of_int_expr expr in
        (*dbg "|!|";
        Environment.print Format.std_formatter d.tenv;
        dbg "";*)
        Abstract1.assign_texpr 
          d.tman 
          d.tabstr 
          (Var.of_string id) 
          (Texpr1.of_expr d.tenv e)
          None 
      with Top -> 
        Abstract1.forget_array
          d.tman
          d.tabstr
          [|Var.of_string id|]
          Params.zero_plane_projection
          
    in {d with tabstr = abstr'}

    

  let assume_ineq ineq d expr1 expr2 = 

    let expr1, expr2 = 
      texpr_of_int_expr expr1,
      texpr_of_int_expr expr2 in

    let expr = 
      let open Texpr1 in
      of_expr d.tenv 
        (Binop (Sub, expr2, expr1, Int, Down)) 
    in

    let cons = Tcons1.make 
      expr
      ineq in

    let cons_ar = Tcons1.array_make d.tenv 1 in

    Tcons1.array_set cons_ar 0 cons ;
    
    let abstr' = Abstract1.meet_tcons_array 
      d.tman
      d.tabstr
      cons_ar
    in 
    {d with tabstr = abstr'} 

    
  let assume_leq = assume_ineq Tcons1.SUPEQ 
  let assume_gt d e1 e2 = assume_ineq Tcons1.SUP d e2 e1


  let mk_binop f d1 d2 = 
    {d1 with tabstr = f d1.tman d1.tabstr d2.tabstr }

  let meet  = mk_binop Abstract1.meet
  let join  = mk_binop Abstract1.join
  let widen = mk_binop Abstract1.widening

  let subset d1 d2 = 
    Abstract1.is_leq d1.tman d1.tabstr d2.tabstr

end
