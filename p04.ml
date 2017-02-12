open Core.Std

let rec length ?(n=0) list =
  match list with
  | [] -> n
  | _ :: xs -> length ~n:(n + 1) xs

let () =
  assert ((length [ "a"; "b"; "c" ]) = 3);
  assert ((length []) = 0)
