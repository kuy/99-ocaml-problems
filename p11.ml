open Core.Std

type 'a rle =
  | One of 'a
  | Many of int * 'a

let to_rle item = function
  | 1 -> One item
  | count -> Many (count, item)

let encode list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (to_rle x (count + 1)) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 ((to_rle a (count + 1)) :: acc) t
  in List.rev (aux 0 [] list)

let () =
  assert ((encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]) = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")])
