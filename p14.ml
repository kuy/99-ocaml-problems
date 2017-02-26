open Core.Std

let duplicate list =
  let double x = [x; x] in
  List.fold ~init:[] ~f:(fun l c -> l @ (double c)) list

let () =
  assert ((duplicate ["a";"b";"c";"c";"d"]) = ["a";"a";"b";"b";"c";"c";"c";"c";"d";"d"])
