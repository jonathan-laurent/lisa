(** CONSTS.ML
    A simple abstract domain for rationnals variables
*)

module M : Value_domain.S = struct

  type t = 
  | Top
  | Bottom
  | Const of Q.t

  let top = Top
  let bottom = Bottom

  let const i = Const (Q.of_int i)

  let rand a b = 
    if a < b then Top
    else if a = b then const a
    else Bottom

  let is_bottom x = x = Bottom
  let is_top x = x = Top

  let subset v1 v2 = match v1, v2 with
    | _, Top -> true
    | Bottom, _ -> true
    | Const a, Const b -> a = b
    | _ -> false

  let join v1 v2 = match v1, v2 with
    | Bottom, x | x, Bottom -> x
    | Top, _ | _, Top -> Top
    | Const a, Const b -> 
      if a = b then Const a 
      else Top

  let meet v1 v2 = match v1, v2 with
    | Bottom, _ | _, Bottom -> Bottom
    | Top, x | x, Top -> x
    | Const a, Const b -> 
      if a = b then Const a 
      else Bottom

  let widen = meet

  let neg = function
    | Bottom -> Bottom
    | Top -> Top
    | Const a -> Const (Q.neg a)

  let mk_binop f b1 b2 = match b1, b2 with
    | Bottom, _ | _, Bottom -> Bottom
    | Top, _ | _, Top -> Top
    | Const a, Const b -> Const (f a b)

  let add = mk_binop Q.add
  let sub = mk_binop Q.sub

  let mul b1 b2 = match b1, b2 with 
    | Const x, Top | Top, Const x when x = Q.zero -> const 0
    | _ -> mk_binop Q.mul b1 b2

  let div = mk_binop Q.div

  let assume_ineq ineq v1 v2 = match v1, v2 with
    | _, Bottom | Bottom, _ -> (Bottom, Bottom)
    | _, Top | Top, _ -> (v1, v2)
    | Const a, Const b ->
      if ineq a b then (v1, v2)
      else (Bottom, Bottom)

  let assume_leq = assume_ineq (<=)
  let assume_gt  = assume_ineq (>)

  let print = function
    | Bottom -> "bottom"
    | Top -> "top"
    | Const c -> "{" ^ Q.to_string c ^ "}"

end
