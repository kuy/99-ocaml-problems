open Core.Std

let rec last list =
  match list with
  | [] -> None
  | x :: [] -> Some x
  | _ :: xs -> last xs

let () =
  assert ((last []) = None);
  assert ((last [ "a"; "b"; "c"; "d" ]) = (Some "d"))
