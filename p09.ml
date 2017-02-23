open Core.Std

let dump_list list =
  List.fold ~init:"" ~f:(fun p l -> p ^ "," ^ (List.fold ~init:"" ~f:(fun p' i -> p' ^ ";" ^ i) l)) list

let pack list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      match acc with
      | (x' :: _) as x'' :: xs' when x = x' -> aux ((x :: x'') :: xs') xs
      | [] | _ -> aux ([x] :: acc) xs
  in List.rev (aux [] list)

let () =
  assert ((pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]) = [["a";"a";"a";"a"]; ["b"]; ["c";"c"]; ["a";"a"]; ["d";"d"]; ["e";"e";"e";"e"]])
