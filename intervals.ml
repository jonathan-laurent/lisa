(** INTERVALS.ML
    An abstract domain for closed intervals with
    rational bounds

    It can be used with integers with the [integer_mode] 
    parameter but division and modulo are not handled in this case
*)

open List
open Utils

module type PARAMS = sig

  (** Should be true if the program works with integers only
      It enables a more precise behaviour for [assume_gt] 

      The division operation doesn't work yet for integers
  *)

  val integer_mode : bool
  
  (** A finite sorted list of bounds. Widening the upper bound
      of an interval causes this upper bound to take the
      value of the smallest greater item in this list. 

      This list has to be sorted *)
    
  val widening_thresholds : int list


  (** Returns bottom when a division by zero is possible
      Ex : 3 / [-1;1] = Bottom  *)
    
  val bottom_on_div_by_zero : bool

end


(** Some standards sets of parameters *)

module Integers : PARAMS = struct
  let integer_mode = true
  let bottom_on_div_by_zero = false
  let widening_thresholds = []
end

module Rationals : PARAMS = struct
  let integer_mode = true
  let bottom_on_div_by_zero = false
  let widening_thresholds = []
end

module Widening_thresholds : PARAMS = struct

  let integer_mode = true
  let bottom_on_div_by_zero = false

  let widening_thresholds =

    let exact_lim   = 10 in
    let step_factor = 2  in
    let step_length = 4  in
    let max_thres   = 10000  in 
    
    let rec gen i s c = 
      if i >= max_thres then []
      else if c >= step_length then gen i (s * step_factor) 0
      else i :: gen (i + s) s (c + 1)
        
    in let l = range 0 (exact_lim - 1) @ (gen exact_lim 2 0) in
       rev (map (fun x -> -x) l) @ l
         
end




module Make (Params : PARAMS) : Value_domain.S = struct

  type bound = 
  | Rat of Q.t
  | Pinf
  | Minf

  (* Empty is the only allowed representation of Bottom *)

  type t = 
  | Empty
  | Interval of (bound * bound)


  let bound_of_int i = Rat (Q.of_int i)

  (** Returns r where
      r < 0 if b1 <= b2, 
      r = 0 if b1 = b2, 
      r > 0 otherwise *)

  let bound_cmp b1 b2 = match b1, b2 with
    | Pinf, Pinf | Minf, Minf -> 0
    | _, Pinf | Minf, _ -> -1
    | _, Minf | Pinf, _ -> 1
    | Rat q1, Rat q2 ->
      Q.compare q1 q2

  let bound_leq b1 b2 = bound_cmp b1 b2 <= 0
  let bound_lt  b1 b2 = bound_cmp b1 b2 <  0
  let bound_eq  b1 b2 = bound_cmp b1 b2 =  0

  let bound_isz b = bound_eq b (bound_of_int 0)

  let bound_min b1 b2 = if bound_leq b1 b2 then b1 else b2
  let bound_max b1 b2 = if bound_leq b1 b2 then b2 else b1

  let bound_maxl = List.fold_left bound_max Minf
  let bound_minl = List.fold_left bound_min Pinf

  let bound_add b1 b2 = match b1, b2 with
    | Rat r1, Rat r2 -> Rat (Q.add r1 r2)

  (* oo + (- oo) is undefined *)
    | Pinf, Minf | Minf, Pinf -> assert false

    | Pinf, _ | _, Pinf -> Pinf
    | Minf, _ | _, Minf -> Minf

  let bound_neg = function
    | Pinf -> Minf
    | Minf -> Pinf
    | Rat r -> Rat (Q.neg r)
      
  let bound_sub b1 b2 = bound_add b1 (bound_neg b2)

  let bound_mul b1 b2 = match b1, b2 with
    | Rat r1, Rat r2 -> Rat (Q.mul r1 r2)
    | Pinf, Minf | Minf, Pinf -> Minf
    | Pinf, Pinf | Minf, Minf -> Pinf
    | Minf, Rat r | Rat r, Minf -> 
      let c = Q.compare r (Q.of_int 0) in
      if c = 0 then bound_of_int 0
      else if c > 0 then Minf
      else Pinf
    | Pinf, Rat r | Rat r, Pinf -> 
      let c = Q.compare r (Q.of_int 0) in
      if c = 0 then bound_of_int 0
      else if c > 0 then Pinf
      else Minf

       
  exception Div_by_zero

  let bound_inv = function
    | Minf | Pinf -> Rat Q.zero
    | Rat r when Q.compare Q.zero r <> 0 -> Rat (Q.inv r)
    | _ -> raise Div_by_zero
        
  
  let mk_interval a b =
    if bound_leq a b && not (a = b && (a = Pinf || b = Pinf))
    then Interval (a, b)
    else Empty









  let top = Interval (Minf, Pinf)

  let bottom = Empty

  let const i = 
    let i' = bound_of_int i in
    Interval (i', i')

  let rand a b = mk_interval (bound_of_int a) (bound_of_int b)

  let is_bottom v = (v = bottom)
  let is_top v = (v = top)

  let subset i1 i2 = match i1, i2 with
    | Empty, _ -> true
    | _, Empty -> false
    | Interval (a1, b1), Interval (a2, b2) ->
      bound_leq a2 a1 && bound_leq b1 b2

  let join i1 i2 = match i1, i2 with
    | Empty, i | i, Empty -> i
    | Interval (a1, b1), Interval (a2, b2) ->
      Interval (bound_min a1 a2, bound_max b1 b2)

  let meet i1 i2 = match i1, i2 with
    | Empty, _ | _, Empty -> Empty
    | Interval (a1, b1), Interval (a2, b2) ->
      mk_interval (bound_max a1 a2) (bound_min b1 b2)

  let widening_thresholds = 
    [Minf] @ (map bound_of_int Params.widening_thresholds) @ [Pinf]

  let rev_widening_thresholds = rev widening_thresholds

  let widen i1 i2 = match i1, i2 with
    | Empty, x | x, Empty -> x
    | Interval (a1, b1), Interval (a2, b2) ->
      let a = 
        if bound_lt a2 a1
        then take_first (fun r -> bound_lt r a2) rev_widening_thresholds
        else a1
      in 
      let b = 
        if bound_lt b1 b2
        then take_first (fun r -> bound_lt b2 r) widening_thresholds
        else b1
      in
      mk_interval a b



  let neg = function
    | Empty -> Empty
    | Interval (a, b) -> Interval (bound_neg b, bound_neg a)

  let strict_binop f = fun i1 i2 -> match i1, i2 with
    | Empty, _ | _, Empty -> Empty
    | Interval b1s, Interval b2s -> f b1s b2s

  let add = strict_binop (fun (a1, b1) (a2, b2) ->
    Interval (bound_add a1 a2, bound_add b1 b2) )

  let sub = strict_binop (fun (a1, b1) (a2, b2) ->
    Interval (bound_sub a1 b2, bound_sub b1 a2) )

  let mul = strict_binop (fun (a1, b1) (a2, b2) ->
    let l =  [
      bound_mul a1 a2; bound_mul a1 b2; 
      bound_mul b1 a2; bound_mul b1 b2] in
    mk_interval (bound_minl l) (bound_maxl l) )


  let contains_zero = subset (Interval (Rat Q.zero, Rat Q.zero))

  let inv = function
    | Empty -> Empty
    | Interval (a, b) as i -> 
      if contains_zero i 
      then 
        (if Params.bottom_on_div_by_zero || (bound_isz a && bound_isz b)
         then bottom
         else if bound_isz a then Interval (bound_of_int 0, Pinf)
         else if bound_isz b then Interval (Minf, bound_of_int 0)
         else top)

      else    
        let a', b' = bound_inv a, bound_inv b in
        Interval (bound_min a' b', bound_max a' b')
        
  
  let div i1 i2 = mul i1 (inv i2)


  let assume_leq i1 i2 = match i1, i2 with
    | _, Empty | Empty, _ -> (Empty, Empty)
    | Interval (a1, b1), Interval (a2, b2) ->
      (mk_interval a1 (bound_min b1 b2), 
       mk_interval (bound_max a1 a2) b2)

        
  let rm_singleton = function
    | Empty -> Empty
    | Interval (a, b) -> 
      if bound_eq a b then Empty
      else Interval(a, b)


  let assume_lt i1 i2 = 

    if Params.integer_mode 
    then 
      let i1_trans', i2' = assume_leq (add i1 (const 1)) i2 in
      (add i1_trans' (const (-1)), i2')

    else
      let i1', i2' = assume_leq i1 i2 in
      match i1', i2' with
      | Interval(a1, b1), Interval (a2, b2) when bound_eq b1 a2 ->
        rm_singleton i1', rm_singleton i2'
      | _ -> i1', i2'


  let assume_gt i1 i2 = Utils.swap_pair (assume_lt i2 i1)

  let print_bound = function
    | Pinf -> "+oo"
    | Minf -> "-oo"
    | Rat r -> Q.to_string r

  let print = function
    | Empty -> "bottom"
    | Interval (b1, b2) -> 
      "[" ^ print_bound b1 ^ ";" ^ print_bound b2 ^ "]"

end




