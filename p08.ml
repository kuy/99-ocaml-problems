open Core.Std

let compress list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      match acc with
      | [x'] | x' :: _ when x = x' -> aux acc xs
      | [] | _ -> aux (x :: acc) xs
  in List.rev (aux [] list)

let () =
  assert ((compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) = ["a";"b";"c";"a";"d";"e"]);
