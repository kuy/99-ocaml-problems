open Core.Std

let is_palindrome list =
  list = List.rev list

let () =
  assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]);
  assert (not(is_palindrome [ "a"; "b" ]))
