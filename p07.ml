open Core.Std

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec aux list r =
    match list with
    | [] -> r
    | One x :: xs -> aux xs (r @ [x])
    | Many xs :: xss -> aux xss (aux xs r)
  in aux list []

let () =
  assert ((flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]) = [ "a"; "b"; "c"; "d"; "e" ]);
