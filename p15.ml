open Core.Std

let repeat x n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (x :: acc) (n - 1)
  in aux [] n

let replicate list n =
  List.fold ~init:[] ~f:(fun l c -> l @ (repeat c n)) list

let () =
  assert ((replicate ["a";"b";"c"] 3) = ["a";"a";"a";"b";"b";"b";"c";"c";"c"])
