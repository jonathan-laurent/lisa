(** NON_RELATIONNAL.ML *)

open Abstract_syntax_tree

module Make(V : Value_domain.S) : Environment_domain.S = struct

  module Map = Mapext.Make
    (struct type t = id
            let compare = compare
     end)

  type env = V.t Map.t

  type t = 
  | Env of env 
  | Bottom

  let strict f = function
    | Bottom -> Bottom
    | Env e -> f e
      
  let is_bottom = function
    | Bottom -> true
    | Env env -> false (* assumes no variable in env is bottom *)

  (* A clever constructor that assures the 
     above invariant is preserved *)

  let mk_env env = 
    if Map.exists (Utils.const V.is_bottom) env
    then Bottom
    else Env env


  (* Pointwise operations *)
    
  let pointwise f bot a b = match a, b with
    | Bottom, x | x, Bottom -> bot x
    | Env m, Env n -> mk_env (Map.map2z (fun _ x y -> f x y) m n)

  let join  = pointwise V.join  (Utils.id)
  let meet  = pointwise V.meet  (Utils.const Bottom)
  let widen = pointwise V.widen (Utils.id)

  let subset a b = match a, b with
    | Bottom, x -> true
    | x, Bottom -> is_bottom x
    | Env e, Env e' -> 
      Map.for_all2z (fun _ x x' -> V.subset x x') e e'

  let init () = Bottom
  let set_bottom _ = Bottom

  let add_variable e id = match e with
    | Bottom -> Env (Map.singleton id V.top)
    | Env e -> Env (Map.add id V.top e)

  let remove_variable e id = match e with
    | Bottom -> Bottom
    | Env e -> Env (Map.remove id e)

  let vars_list = function
    | Bottom -> []
    | Env e -> List.map fst (Map.bindings e)


  let print = function
    | Bottom -> "bottom"
    | Env e ->
      let items = List.map 
        (fun (id, v) -> id ^ " in " ^ (V.print v)) 
        (Map.bindings e)
      in
      "[ " ^ (String.concat ", " items) ^ " ]"


  let rec eval (env : env) = function
   | AST_unary (AST_UNARY_PLUS, e) -> eval env (rm_ext e)
   | AST_unary (AST_UNARY_MINUS, e) -> V.neg (eval env (rm_ext e))    
   | AST_unary (AST_NOT, e) -> assert false

   | AST_binary (op, lhs, rhs) ->
    (
      let valops = 
        [(AST_PLUS, V.add) ; (AST_MINUS, V.sub) ; 
         (AST_MULTIPLY, V.mul) ; (AST_DIVIDE, V.div)] in

      try (List.assoc op valops) 
            (eval env (rm_ext lhs)) (eval env (rm_ext rhs))

      (* Modulo and binary operations are not implemented *)
      with _ -> assert false
    )

   | AST_identifier id -> Map.find (rm_ext id) env

   | AST_int_const c -> V.const (int_of_string (rm_ext c))
   | AST_int_rand (a, b) -> V.rand 
     (int_of_string (rm_ext a)) (int_of_string (rm_ext b))

   | AST_bool_const _ -> assert false
   | AST_expr_call _ -> assert false
      

  let assign env id expr = match env with
    | Bottom -> Bottom
    | Env env -> mk_env (Map.add id (eval env expr) env)


  (* We handle two cases : 
      var <= expr || expr <= var
      var <= var
  *)
  let assume_ineq ineq env e1 e2 = match env with
    | Bottom -> Bottom
    | Env env -> 

      (match e1, e2 with
      | AST_identifier id1, AST_identifier id2 ->

        let id1, id2 = rm_ext id1, rm_ext id2 in
        let v1, v2 = 
          ineq (Map.find id1 env) (Map.find id2 env) in

        mk_env (Map.add id1 v1 (Map.add id2 v2 env))

      | AST_identifier id, expr ->

        let id = rm_ext id in
        let v, _ = 
          ineq (Map.find id env) (eval env expr) in

        mk_env (Map.add id v env)
        

      | expr, AST_identifier id ->

        let id = rm_ext id in
        let _, v = 
          ineq (eval env expr) (Map.find id env) in

        mk_env (Map.add id v env)

      | expr1, expr2 ->

        let v1, v2 = 
          eval env expr1, eval env expr2 in
        let v1', v2' = ineq v1 v2 in
        if V.is_bottom v1' || V.is_bottom v2'
        then Bottom
        else Env env

      )

  let assume_leq = assume_ineq V.assume_leq
  let assume_gt  = assume_ineq V.assume_gt
 
end
