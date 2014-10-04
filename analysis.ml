(** ANALYSIS.ML
    Uses the generic [Environment_interface] in order
    to check a program
*)

module type PARAMS = sig
  val widening_delay : int
  val unrolling_factor : int
  val decreasing_iterations : int
  val auto_phantom_vars : bool

  val verbose_unrolled_statements : bool
end

module Make (E : Environment_domain.S) (Params : PARAMS) = struct

  open Abstract_syntax_tree
  open List
  open Utils

  module ELib = Environment_domain.Lib(E)
  open ELib
  open E

  (* These types encode the informations
     delivered by the analyser *)

  type log_entry = 
  | Assertion_failure of extent
  | Print_env of E.t * extent

  type result = (log_entry Queue.t) * E.t

  let print_result ff (log, output_env) = 

    let open Format in

    let print_log_line = function
      | Assertion_failure ext -> 
        fprintf ff "%s: ERROR: assertion failure\n"
          (print_one_line_extent ext)
      | Print_env (env, ext) -> 
        fprintf ff "%s: %s\n" 
          (print_one_line_extent ext) 
          (E.print env)
    in
    Queue.iter print_log_line log ;
    fprintf ff "Output: %s" (print output_env)


  (* The PRINT and ASSERT messages are queued in 
     this global structure 

     The vote system is a dirty trick aimed at 
     unsetting logging while analysing loops
  *)
      
  let log = Queue.create ()

  let log_votes = ref 0
    
  let clear_log_votes () = log_votes := 0
  let log_upvote () = incr log_votes
  let log_downvote () = decr log_votes

  let logw (e : log_entry) = 
    if !log_votes >= 0 then Queue.push e log




  (** Returns a pair (true_env, false_env)
      where true_env is env with the constraint that cond = true
      (same for false_env)  *)
    
  let process_cond env = 

    let rec compute_not = swap_pair

    and compute_or (ltrue, lfalse) (rtrue, rfalse) = 
      (join ltrue rtrue, meet lfalse rfalse)

    and compute_and (ltrue, lfalse) (rtrue, rfalse) = 
      (meet ltrue rtrue, join lfalse rfalse)

    and process_equal lhs rhs =
      compute_and 
        (process_leq lhs rhs) 
        (process_leq rhs lhs)

    and process_leq lhs rhs =
      (assume_leq env lhs rhs, assume_gt env lhs rhs)

    and process_geq lhs rhs = process_leq rhs lhs

    and process cond = 
      (match cond with
      | AST_bool_const b ->
        if b then (env, set_bottom env) else (set_bottom env, env)

      | AST_unary (AST_NOT, expr) -> 
        compute_not (process (rm_ext expr))

      | AST_binary (op, lhs, rhs) ->

        let lhs, rhs = rm_ext lhs, rm_ext rhs in
        (match op with

        | AST_LESS_EQUAL -> process_leq lhs rhs
        | AST_GREATER_EQUAL -> process_geq lhs rhs
        | AST_AND -> compute_and (process lhs) (process rhs)
        | AST_OR -> compute_or (process lhs) (process rhs)
        | AST_EQUAL -> process_equal lhs rhs
        | AST_NOT_EQUAL -> compute_not (process_equal lhs rhs)
        | AST_LESS -> compute_not (process_geq lhs rhs)
        | AST_GREATER -> compute_not (process_leq lhs rhs)

        | _ -> assert false

        )

      (* The expression must be boolean *)
      | _ -> assert false 
      )

    in process

  let assume_cond env cond = fst (process_cond env cond)
  let assume_not_cond env cond = snd (process_cond env cond)
    






  (* IMPORTANT : This function assumes local variables 
     have distinct names *)

  let process_toplevel_stat env stat = 

    (* This stack is aimed at registering the position
       of the current statement in the block structure
       and the local variables defined at each level
       of this structure *)

    let local_vars : (id list Stack.t) = Stack.create () in
    let push_local_var var = 
      Stack.push (var :: Stack.pop local_vars) local_vars in



    let rec process_stat env (expr, ext) = 
      match expr with
      | AST_block stats ->
        Stack.push [] local_vars ; 
        let env' = 
          fold_left process_stat env stats in
        
        (* We remove the local variables defined at this level *)
        fold_left remove_variable env' (Stack.pop local_vars)

      | AST_assign (id, expr) ->
        let id, expr = rm_ext id, rm_ext expr in
        assign env id expr

      | AST_if (cond, then_i, else_i) ->
        process_if env (rm_ext cond) then_i else_i
      
      | AST_while (cond, stat) ->

        (* Logging will be enabled again 
           after the loop invariant is computed *)
        if not Params.verbose_unrolled_statements then log_downvote (); 
        unroll_then_process_loop 
          env (rm_ext cond) 
          stat 
          (Params.unrolling_factor)

      | AST_local (typ, var_init) ->
        if (rm_ext typ = AST_TYP_BOOL) then assert false
        else

          let register_var env (id, init) = 
            let id = rm_ext id in
            if List.mem id (vars_list env) 
            then env
            else
              (let env = add_variable env id in
               push_local_var id ;
               match init with
               | None -> env
               | Some expr -> assign env id (rm_ext expr)
              )
          in

          fold_left register_var env var_init
      
      | AST_HALT -> env
      
      | AST_assert cond ->
        let true_env, false_env = process_cond env (rm_ext cond) in
        if not (E.is_bottom false_env)
        then 
          begin
            logw (Assertion_failure ext) ;
            true_env
          end 

        else env
        
      | AST_print vars -> 
        let vars = 
          if vars <> [] 
          then List.map rm_ext vars 
          else E.vars_list env
        in
        logw (Print_env (filter_vars vars env, ext)) ; env

      | _ -> assert false
      

    and process_if env cond then_i else_i = 
      let then_env, else_env = process_cond env cond in
      let e = process_stat then_env then_i in
      let e' = do_option else_env (process_stat else_env) else_i in
      join e e'


    and unroll_then_process_loop env cond stat = function
      | 0 -> process_loop env cond stat       
      | n -> 
        let (true_env, false_env) = process_cond env cond in
        join
          false_env
          (unroll_then_process_loop 
             (process_stat true_env stat) 
             cond stat (n - 1)
          )

    and process_loop unrolled_env cond stat = 

      if Params.verbose_unrolled_statements then log_downvote ();

      let invariant = 
        compute_loop_invariant unrolled_env cond stat in

      (* In order to log PRINT and ASSERT directives *)
      log_upvote ();
      ignore (process_stat invariant stat);
      assume_not_cond invariant cond

        
    and compute_loop_invariant env cond stat = 

      (* We have to sur-approximate lfp f *)
      let f x = join env (process_stat (assume_cond x cond) stat) in

      let rec iter env i = 
        let merge = if i > 0 then E.join else E.widen in
        let env' = merge env (f env) in
        if E.subset env' env then
          env
        else
          iter env' (i - 1)
      in

      let rec use_decr_its env = function
        | 0 -> env
        | n ->
          let env' = f env in
          if subset env' env 
          then use_decr_its env' (n - 1)
          else env
      in
      let after_widening = iter env Params.widening_delay in
      use_decr_its 
        after_widening Params.decreasing_iterations
      
  
    in
    Stack.push [] local_vars ; process_stat env stat

      




  let process_toplevel_item env = function
    | AST_stat stat -> process_toplevel_stat env stat
    | AST_fun_decl _ -> assert false

  let process_prog env prog =
    fold_left process_toplevel_item env (rm_ext prog)
   

  (** Launch an analysis of [prog] and print the result 
      in the formatter [ff]
  *)

  let run ff (prog : prog) =

    Queue.clear log;
    clear_log_votes ();
    Ast_transformations.clear_phantoms_counter ();

    let prog = 
      if Params.auto_phantom_vars then
        Ast_transformations.add_loop_phantoms prog
      else
        prog in

    Format.print_flush ();
    let env = process_prog (init ()) prog in
    print_result ff (log, env)
    
end
