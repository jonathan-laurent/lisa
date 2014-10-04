(** UTILS.ML
    Some nice functions that could have been in
    the standard library
*)


let ( **> ) f x = f x

let ( % ) g f x = g (f x)

let const x _ = x

let id x = x

let swap_pair (x, y) = (y, x)

let rec range a b = if a > b then [] else a :: (range (a + 1) b)

let rec take_first f = function
  | [] -> raise Not_found
  | x :: xs -> if f x then x else take_first f xs

let rec list_remove e = function
  | [] -> []
  | x::xs -> 
    let tl = list_remove e xs in
    if x = e then tl else x :: tl

let list_diff l l' = List.filter (fun x -> not (List.mem x l')) l

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let get_option default = function
  | None -> default
  | Some x -> x

let do_option default f = function
  | None -> default
  | Some x -> f x


let rec self_compose f n x = match n with
  | 0 -> x
  | _ -> f (self_compose f (n-1) x)


let print_file ff filename = 
  try
    let ic = open_in filename in
    try
      while(true) do
        Format.fprintf ff "%s\n" (input_line ic);
      done
    with _ -> close_in ic
  with _ -> ()
      

let dbg_counter = ref 0

let dbg s = 
  Format.fprintf Format.std_formatter "%d. %s\n" !dbg_counter s;
  Format.print_flush ();
  incr dbg_counter
