open Core.Std

let rec at k list =
  match (k, list) with
  | (_, []) -> None
  | (1, x :: _) -> Some x
  | (k, _ :: xs) -> at (k - 1) xs

let () =
  assert ((at 3 [ "a"; "b"; "c"; "d" ]) = (Some "c"));
  assert ((at 3 [ "a" ]) = None)
