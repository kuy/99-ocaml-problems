open Core.Std

let rev list =
  let rec aux list r =
    match list with
    | [] -> r
    | x :: xs -> aux xs (x :: r)
  in aux list []

let () =
  assert ((rev [ "a"; "b"; "c" ]) = [ "c"; "b"; "a" ]);
  assert ((rev [ "1"; "2"; "3"; "4" ]) = [ "4"; "3"; "2"; "1" ]);
