open Core.Std

let encode list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      match acc with
      | [(n, x')] when x = x' -> aux [(n + 1, x)] xs
      | (n, x') :: xs' when x = x' -> aux ((n + 1, x) :: xs') xs
      | [] | _ -> aux ((1, x) :: acc) xs
  in List.rev (aux [] list)

let () =
  assert ((encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])
