open Core.Std

let rec last_two list =
  match list with
  | [] | [_] -> None
  | x :: [y] -> Some (x, y)
  | _ :: xs -> last_two xs

let () =
  assert ((last_two [ "a"; "b"; "c"; "d" ]) = (Some ("c", "d")));
  assert ((last_two [ "a" ]) = None)
