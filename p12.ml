open Core.Std

type 'a rle =
  | One of 'a
  | Many of int * 'a

let repeat n x =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (x :: acc) (n - 1)
  in aux [] n

let expand = function
  | One x -> [x]
  | Many (n, x) -> repeat n x

let decode list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux ((expand x) @ acc) xs
  in List.rev (aux [] list)

let () =
  assert ((repeat 0 "x") = []);
  assert ((repeat 1 "x") = ["x"]);
  assert ((repeat 5 "x") = ["x"; "x"; "x"; "x"; "x"]);

  assert ((expand (One "x")) = ["x"]);
  assert ((expand (Many (4, "x"))) = ["x"; "x"; "x"; "x"]);

  assert ((decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]) = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])
